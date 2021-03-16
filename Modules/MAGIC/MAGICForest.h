

static void
AddMagicForestModule(mobius_model *Model)
{
	BeginModule(Model, "MAGIC Forest", "_dev");
	
	SetModuleDescription(Model, R""""(
Forest growth driver module developed as part of the CatchCAN project.
)"""");
	
	auto Dimensionless   = RegisterUnit(Model);
	auto DegreesCelsius	 = RegisterUnit(Model, "°C");
	//auto MgPerL          = RegisterUnit(Model, "mg/l");
	auto MPerTs          = RegisterUnit(Model, "m/month");
	auto MmPerTs         = RegisterUnit(Model, "mm/month");
	auto MEqPerM3        = RegisterUnit(Model, "meq/m3");
	auto MMolPerM3       = RegisterUnit(Model, "mmol/m3");
	auto MEqPerM2PerTs   = RegisterUnit(Model, "meq/m2/month");
	auto MMolPerM2PerTs  = RegisterUnit(Model, "mmol/m2/month");
	auto MEqPerM2PerYear = RegisterUnit(Model, "meq/m2/year");
	auto YearPerTs       = RegisterUnit(Model, "year/month");
	auto Percent         = RegisterUnit(Model, "%");
	
	//auto Compartment             = GetIndexSetHandle(Model, "Compartment");
	auto Compartment             = RegisterIndexSet(Model, "Compartment");
	
	auto Climate                 = RegisterParameterGroup(Model, "Climate", Compartment);
	
	
	auto PartialPressureCO2Par   = RegisterParameterDouble(Model, Climate, "CO2 partial pressure", Percent, 0.3, 0.1, 2.0, "Default value for timesteps where no input series value is provided");
	auto OAConcentrationPar      = RegisterParameterDouble(Model, Climate, "Organic acid concentration", MMolPerM3, 0.0, 0.0, 200.0, "Default value for timesteps where no input series value is provided");
	auto MinCompartmentTemp      = RegisterParameterDouble(Model, Climate, "Minimal compartment temperature", DegreesCelsius, 0.0, -10.0, 10.0);

	
	auto Weathering              = RegisterParameterGroup(Model, "Weathering", Compartment);
	
	auto CaWeathering            = RegisterParameterDouble(Model, Weathering, "Ca weathering", MEqPerM2PerYear, 0.0, 0.0, 200.0);
	auto MgWeathering            = RegisterParameterDouble(Model, Weathering, "Mg weathering", MEqPerM2PerYear, 0.0, 0.0, 200.0);
	auto NaWeathering            = RegisterParameterDouble(Model, Weathering, "Na weathering", MEqPerM2PerYear, 0.0, 0.0, 200.0);
	auto KWeathering             = RegisterParameterDouble(Model, Weathering, "K weathering", MEqPerM2PerYear, 0.0, 0.0, 200.0);
	auto NH4Weathering           = RegisterParameterDouble(Model, Weathering, "NH4 weathering", MEqPerM2PerYear, 0.0, 0.0, 200.0);
	auto SO4Weathering           = RegisterParameterDouble(Model, Weathering, "SO4 weathering", MEqPerM2PerYear, 0.0, 0.0, 200.0);
	auto ClWeathering            = RegisterParameterDouble(Model, Weathering, "Cl weathering", MEqPerM2PerYear, 0.0, 0.0, 200.0);
	auto NO3Weathering           = RegisterParameterDouble(Model, Weathering, "NO3 weathering", MEqPerM2PerYear, 0.0, 0.0, 200.0);
	auto FWeathering             = RegisterParameterDouble(Model, Weathering, "F weathering", MEqPerM2PerYear, 0.0, 0.0, 200.0);
	
	
	

	
	/*
	auto CAndN                   = RegisterParameterGroup(Model, "Carbon and Nitrogen", Compartment);
	
	auto DecompR0                = RegisterParameterDouble(Model, CAndN, "C decomposition at 0°C", MMolPerM2PerTs, 0.0, 0.0, 1000.0);
	auto DecompQ10               = RegisterParameterDouble(Model, CAndN, "C decomposition Q10", Dimensionless, 1.0, 1.0, 5.0);
	auto UptakeR0                = RegisterParameterDouble(Model, CAndN, "N uptake at 0°C", MMolPerM2PerTs, 0.0, 0.0, 1000.0);
	auto UptakeQ10               = RegisterParameterDouble(Model, CAndN, "N uptake Q10", Dimensionless, 1.0, 1.0, 5.0);
	auto NO3UptakeScale          = RegisterParameterDouble(Model, CAndN, "NO3 uptake scale", Dimensionless, 1.0, 0.1, 10.0);
	auto NH4UptakeScale          = RegisterParameterDouble(Model, CAndN, "NH4 uptake scale", Dimensionless, 1.0, 0.1, 10.0);
	auto LitterCN                = RegisterParameterDouble(Model, CAndN, "Litter C/N", Dimensionless, 50.0, 0.1, 200.0);
	*/
	
	auto AirTemperature          = RegisterInput(Model, "Air temperature", DegreesCelsius);
	auto Precipitation           = RegisterInput(Model, "Precipitation", MmPerTs);
	
	auto PartialPressureCO2In    = RegisterInput(Model, "CO2 partial pressure", Percent, true);           //NOTE: These are auto-cleared to NaN for missing values
	auto OAConcentrationIn       = RegisterInput(Model, "Organic acid concentration", MMolPerM3, true);   // ^
	
	auto CaPrecipConc            = RegisterInput(Model, "Ca conc in precip", MEqPerM3);
	auto MgPrecipConc            = RegisterInput(Model, "Mg conc in precip", MEqPerM3);
	auto NaPrecipConc            = RegisterInput(Model, "Na conc in precip", MEqPerM3);
	auto KPrecipConc             = RegisterInput(Model, "K conc in precip", MEqPerM3);
	auto NH4PrecipConc           = RegisterInput(Model, "NH4 conc in precip", MEqPerM3);
	auto SO4PrecipConc           = RegisterInput(Model, "SO4 conc in precip", MEqPerM3);
	auto ClPrecipConc            = RegisterInput(Model, "Cl conc in precip", MEqPerM3);
	auto NO3PrecipConc           = RegisterInput(Model, "NO3 conc in precip", MEqPerM3);
	auto FPrecipConc             = RegisterInput(Model, "F conc in precip", MEqPerM3);
	
	
	
	
	
	auto FractionOfYear          = RegisterEquation(Model, "Fraction of year", YearPerTs);
	
	/*
	auto Decomposition           = RegisterEquation(Model, "Organic C decomposition", MMolPerM2PerTs);
	auto UptakeBaseline          = RegisterEquation(Model, "N uptake baseline", MMolPerM2PerTs);
	auto DesiredNO3Uptake        = RegisterEquation(Model, "Desired NO3 uptake", MMolPerM2PerTs);
	auto DesiredNH4Uptake        = RegisterEquation(Model, "Desired NH4 uptake", MMolPerM2PerTs);
	auto LitterC                 = RegisterEquation(Model, "Organic C litter", MMolPerM2PerTs);
	*/
	
	auto MgDeposition            = RegisterEquation(Model, "Mg deposition", MEqPerM2PerTs);
	auto NH4Deposition           = RegisterEquation(Model, "NH4 deposition", MEqPerM2PerTs);
	auto NO3Deposition           = RegisterEquation(Model, "NO3 deposition", MEqPerM2PerTs);
	auto ClDeposition            = RegisterEquation(Model, "Cl deposition", MEqPerM2PerTs);
	auto NaDeposition            = RegisterEquation(Model, "Na deposition", MEqPerM2PerTs);
	auto KDeposition             = RegisterEquation(Model, "K deposition", MEqPerM2PerTs);
	auto SO4Deposition           = RegisterEquation(Model, "SO4 deposition", MEqPerM2PerTs);
	auto CaDeposition            = RegisterEquation(Model, "Ca deposition", MEqPerM2PerTs);
	auto FDeposition             = RegisterEquation(Model, "F deposition", MEqPerM2PerTs);
	
	
	// These are required by the MAGIC Core:
	auto Discharge               = RegisterEquation(Model, "Discharge", MPerTs);
	auto Temperature             = RegisterEquation(Model, "Temperature", DegreesCelsius);
	auto PartialPressureCO2      = RegisterEquation(Model, "CO2 partial pressure", Percent);
	auto OAConcentration         = RegisterEquation(Model, "Organic acid concentration", MMolPerM3);
	
	auto CaExternalFlux          = RegisterEquation(Model, "Sum of Ca fluxes not related to discharge", MEqPerM2PerTs);
	auto MgExternalFlux          = RegisterEquation(Model, "Sum of Mg fluxes not related to discharge", MEqPerM2PerTs);
	auto NaExternalFlux          = RegisterEquation(Model, "Sum of Na fluxes not related to discharge", MEqPerM2PerTs);
	auto KExternalFlux           = RegisterEquation(Model, "Sum of K fluxes not related to discharge", MEqPerM2PerTs);
	auto NH4ExternalFlux         = RegisterEquation(Model, "Sum of NH4 fluxes not related to discharge", MEqPerM2PerTs);
	auto SO4ExternalFlux         = RegisterEquation(Model, "Sum of SO4 fluxes not related to discharge", MEqPerM2PerTs);
	auto ClExternalFlux          = RegisterEquation(Model, "Sum of Cl fluxes not related to discharge", MEqPerM2PerTs);
	auto NO3ExternalFlux         = RegisterEquation(Model, "Sum of NO3 fluxes not related to discharge", MEqPerM2PerTs);
	auto FExternalFlux           = RegisterEquation(Model, "Sum of F fluxes not related to discharge", MEqPerM2PerTs);
	
	
	// From WASMOD:
	auto Runoff                  = GetEquationHandle(Model, "Runoff");
	
	
	EQUATION(Model, FractionOfYear,
		return (double)CURRENT_TIME().StepLengthInSeconds / (86400.0*(double)CURRENT_TIME().DaysThisYear);
	)
	
	EQUATION(Model, PartialPressureCO2,
		double par = PARAMETER(PartialPressureCO2Par);
		double in  = INPUT(PartialPressureCO2In);
		if(std::isfinite(in)) return in;
		return par;
	)
	
	EQUATION(Model, OAConcentration,
		double par = PARAMETER(OAConcentrationPar);
		double in = INPUT(OAConcentrationIn);
		if(std::isfinite(in)) return in;
		return par;
	)
	
	
	EQUATION(Model, Discharge,
		//TODO: We need to do something more sophisticated in how we do routing between multiple compartments, and how we handle water bodies!
		return RESULT(Runoff) * 1e-3;
	)
	
	
	EQUATION(Model, Temperature,
		return Max(INPUT(AirTemperature), PARAMETER(MinCompartmentTemp));
	)
	
	
	/*
	EQUATION(Model, Decomposition,
		return PARAMETER(DecompR0) * std::pow(PARAMETER(DecompQ10), RESULT(Temperature) * 0.1);
	)
	
	EQUATION(Model, UptakeBaseline,
		return PARAMETER(UptakeR0) * std::pow(PARAMETER(UptakeQ10), RESULT(Temperature) * 0.1);
	)
	
	EQUATION(Model, DesiredNO3Uptake,
		return RESULT(UptakeBaseline) * PARAMETER(NO3UptakeScale);
	)
	
	EQUATION(Model, DesiredNH4Uptake,
		return RESULT(UptakeBaseline) * PARAMETER(NH4UptakeScale);
	)
	
	EQUATION(Model, LitterC,
		return (RESULT(DesiredNO3Uptake) + RESULT(DesiredNH4Uptake))*PARAMETER(LitterCN);
	)
	*/
	
	
	EQUATION(Model, CaDeposition,
		return INPUT(CaPrecipConc) * INPUT(Precipitation) * 1e-3;
	)
	
	EQUATION(Model, MgDeposition,
		return INPUT(MgPrecipConc) * INPUT(Precipitation) * 1e-3;
	)
	
	EQUATION(Model, NaDeposition,
		return INPUT(NaPrecipConc) * INPUT(Precipitation) * 1e-3;
	)

	EQUATION(Model, KDeposition,
		return INPUT(KPrecipConc) * INPUT(Precipitation) * 1e-3;
	)
	
	EQUATION(Model, NH4Deposition,
		return INPUT(NH4PrecipConc) * INPUT(Precipitation) * 1e-3;
	)
	
	EQUATION(Model, SO4Deposition,
		return INPUT(SO4PrecipConc) * INPUT(Precipitation) * 1e-3;
	)
	
	EQUATION(Model, NO3Deposition,
		return INPUT(NO3PrecipConc) * INPUT(Precipitation) * 1e-3;
	)
	
	EQUATION(Model, ClDeposition,
		return INPUT(ClPrecipConc) * INPUT(Precipitation) * 1e-3;
	)
	
	EQUATION(Model, FDeposition,
		return INPUT(FPrecipConc) * INPUT(Precipitation) * 1e-3;
	)
	
	
	EQUATION(Model, CaExternalFlux,
		return RESULT(CaDeposition) + RESULT(FractionOfYear)*PARAMETER(CaWeathering);
	)
	
	EQUATION(Model, MgExternalFlux,
		return RESULT(MgDeposition) + RESULT(FractionOfYear)*PARAMETER(MgWeathering);
	)
	
	EQUATION(Model, NaExternalFlux,
		return RESULT(NaDeposition) + RESULT(FractionOfYear)*PARAMETER(NaWeathering);
	)
	
	EQUATION(Model, KExternalFlux,
		return RESULT(KDeposition) + RESULT(FractionOfYear)*PARAMETER(KWeathering);
	)
	
	EQUATION(Model, NH4ExternalFlux,
		return RESULT(NH4Deposition) + RESULT(FractionOfYear)*PARAMETER(NH4Weathering);
	)
	
	EQUATION(Model, SO4ExternalFlux,
		return RESULT(SO4Deposition) + RESULT(FractionOfYear)*PARAMETER(SO4Weathering);
	)
	
	EQUATION(Model, NO3ExternalFlux,
		return RESULT(NO3Deposition) + RESULT(FractionOfYear)*PARAMETER(NO3Weathering);
	)
	
	EQUATION(Model, ClExternalFlux,
		return RESULT(ClDeposition) + RESULT(FractionOfYear)*PARAMETER(ClWeathering);
	)
	
	EQUATION(Model, FExternalFlux,
		return RESULT(FDeposition) + RESULT(FractionOfYear)*PARAMETER(FWeathering);
	)
	
	
	EndModule(Model);
}