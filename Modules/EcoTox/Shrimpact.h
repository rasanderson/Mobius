


// This model is based on
// S. Jannicke Moe & al. 2019, Effects of an aquaculture pesticide (diflubenzuron) on non-target shrimp populations: Extrapolation from laboratory experiments to the risk of population decline.  Ecological Modelling 413, 108833


void AddShrimpactModel(mobius_model *Model)
{
	BeginModule(Model, "SHRIMPACT", "0.1");
	
	SetModuleDescription(Model, R""""(
This is a shrimp population model that can react to environmental stressors. It has been published as:

[S. Jannicke Moe & al. 2019, Effects of an aquaculture pesticide (diflubenzuron) on non-target shrimp populations: Extrapolation from laboratory experiments to the risk of population decline.  Ecological Modelling 413, 108833](https://doi.org/10.1016/j.ecolmodel.2019.108833)
)"""");
	
	auto Dimensionless        = RegisterUnit(Model);
	auto Individuals          = RegisterUnit(Model, "ind/100m2");
	auto IndividualsPerSeason = RegisterUnit(Model, "ind/100m2/season");
	auto PerInd               = RegisterUnit(Model, "1/(ind/100m2)");
	auto Season               = RegisterUnit(Model, "season");
	auto Year                 = RegisterUnit(Model, "year");
	auto Percent              = RegisterUnit(Model, "%");
	
	auto AgeClass = RegisterIndexSet(Model, "Age class");
	
	auto GeneralParam = RegisterParameterGroup(Model, "General");
	auto AgeParam     = RegisterParameterGroup(Model, "Age class", AgeClass);
	
	auto StrengthOfDensityDependence = RegisterParameterDouble(Model, GeneralParam, "Strength of density dependence (a)", PerInd, 0.01, 1e-6, 1);
	auto DegreeOfCompensation        = RegisterParameterDouble(Model, GeneralParam, "Degree of compensation (b)", Dimensionless, 1.0, 0.1, 10.0);
	auto FirstAdultClass             = RegisterParameterUInt(Model, GeneralParam, "First adult age class", Season, 6, 1, 100);
	
	auto BadYearType                 = RegisterParameterEnum(Model, GeneralParam, "Bad year type", {"None", "Once", "Multiple"}, "None");
	auto BadYearNumber               = RegisterParameterUInt(Model, GeneralParam, "Bad year", Year, 2040, 1000, 3000, "What year is the bad year if it happens only once.");
	auto BadYearChance               = RegisterParameterDouble(Model, GeneralParam, "Bad year chance", Percent, 25.0, 0.0, 100.0, "What is the chance that a year is bad if there are multiple bad years.");
	auto BadYearSurvivalReduction    = RegisterParameterDouble(Model, GeneralParam, "Reduction of larva survival in bad year", Dimensionless, 0.5, 0.0, 1.0, "Multiplier to larva survival rate in a bad year");
	auto VariationInSurvival         = RegisterParameterDouble(Model, GeneralParam, "Variation in survival", Dimensionless, 1.0, 0.8, 1.2, "Exponent to survival rate. Used for changing rate when running the model stochastically"); 
	auto Toxicity                    = RegisterParameterDouble(Model, GeneralParam, "Toxicity multiplier", Dimensionless, 1.0, 0.1, 1.0, "Multiplier to survival rate of non-larvae caused by toxicity");
	auto LarvaToxicity               = RegisterParameterDouble(Model, GeneralParam, "Larva toxicity multiplier", Dimensionless, 1.0, 0.1, 1.0, "Multiplier to survival rate of larvae caused by toxicity");
	
	
	auto BaseSurvivalRate            = RegisterParameterDouble(Model, AgeParam, "Base survival rate", Dimensionless, 0.9, 0.0, 1.0);
	auto Fertility                   = RegisterParameterDouble(Model, AgeParam, "Fertility", Dimensionless, 0.0, 0.0, 1.0);
	
	auto InitialPopulationSize       = RegisterParameterDouble(Model, AgeParam, "Initial population size", Individuals, 500.0, 0.0, 5000.0);
	
	
	
	auto Birth                = RegisterEquation(Model, "Birth", IndividualsPerSeason);
	auto TotalBirth           = RegisterEquationCumulative(Model, "Total birth", Birth, AgeClass);
	auto SurvivalRate         = RegisterEquation(Model, "Survival rate", Dimensionless);
	
	auto PopulationSize       = RegisterEquation(Model, "Population size", Individuals);
	SetInitialValue(Model, PopulationSize, InitialPopulationSize);
	
	auto TotalAdultPopulation = RegisterEquation(Model, "Total adult population", Individuals);
	
	EQUATION(Model, SurvivalRate,
		index_t Age           = CURRENT_INDEX(AgeClass);
		double survivalRate   = PARAMETER(BaseSurvivalRate);
		double lastRate       = LAST_RESULT(SurvivalRate);
		double toxicity       = PARAMETER(Toxicity);
		double larvaetoxicity = PARAMETER(LarvaToxicity);
		
		survivalRate = std::pow(survivalRate, PARAMETER(VariationInSurvival));
		if(Age != FIRST_INDEX(AgeClass)) survivalRate *= toxicity;
		else                             survivalRate *= larvaetoxicity;
		
		double badYearReduction = PARAMETER(BadYearSurvivalReduction);
		u64    badYearNumber    = PARAMETER(BadYearNumber);
		double badYearChance    = PARAMETER(BadYearChance);
		
		u64 badtype = PARAMETER(BadYearType);
		
		if(CURRENT_TIME().Year == 2020) return 1;
		
		if(badtype > 0 && Age == FIRST_INDEX(AgeClass))
		{
			if(badtype == 1 && CURRENT_TIME().Year == badYearNumber)
				survivalRate *= badYearReduction;
			else if(badtype == 2)
			{
				if(CURRENT_TIME().Month == 1)
				{
					if(UNIFORM_RANDOM_DOUBLE(0, 100) <= badYearChance)
						survivalRate *= badYearReduction;
				}
				else
					survivalRate = lastRate;
			}
		}
		
		return survivalRate;
	)
	
	EQUATION(Model, PopulationSize,
		index_t Age    = CURRENT_INDEX(AgeClass);
		double a       = PARAMETER(StrengthOfDensityDependence);
		double b       = PARAMETER(DegreeOfCompensation);
		double NA      = LAST_RESULT(TotalAdultPopulation);
		double birth   = LAST_RESULT(TotalBirth);
		u64 firstAdult = PARAMETER(FirstAdultClass)-1;  //0-based indexing
		
		if(Age == FIRST_INDEX(AgeClass))
			return birth;
		
		double survival = RESULT(SurvivalRate, Age-1);
		double Size = survival * LAST_RESULT(PopulationSize, Age-1);
		
		if(Age >= INDEX_NUMBER(AgeClass, firstAdult))
			Size /= (1.0 + pow(a*NA, b));
		
		return Size;
	)
	
	EQUATION(Model, TotalAdultPopulation,
		double Tot = 0.0;
		u64 firstAdult = PARAMETER(FirstAdultClass)-1; //0-based indexing
		for(index_t Age = INDEX_NUMBER(AgeClass, firstAdult); Age < INDEX_COUNT(AgeClass); ++Age)
			Tot += RESULT(PopulationSize, Age);
		
		return Tot;
	)
	
	EQUATION(Model, Birth,
		return RESULT(PopulationSize) * PARAMETER(Fertility);
	)
	
	EndModule(Model);
}