
#if !defined(PERSIST_MODEL_H)

static void
AddPersistModel(mobius_model *Model)
{
	BeginModule(Model, "PERSiST", "1.4");
	
	SetModuleDescription(Model, R""""(
This is an implementation of

[M. N. Futter et. al. 2014, PERSiST: a flexible rainfall-runoff modelling toolkit for use with the INCA family of models, Hydrol. Earth Syst. Sci., 18, 855-873](https://doi.org/10.5194/hess-18-855-2014)

This implementation is slightly simpler than specified in the paper, removing processes describing infiltration and inundation from the river to land.

New to version 1.4:
Computation of reach flow based on the Manning flow equations.	
)"""");
	
	auto Mm                   = RegisterUnit(Model, "mm");
	auto MmPerDay             = RegisterUnit(Model, "mm/day");
	auto DegreesCelsius       = RegisterUnit(Model, "°C");
	auto MmPerDegreePerDay    = RegisterUnit(Model, "mm/°C/day");
	auto MmSWE                = RegisterUnit(Model, "mm SWE");
	auto Dimensionless        = RegisterUnit(Model);
	auto Days                 = RegisterUnit(Model, "days");
	auto Meters               = RegisterUnit(Model, "m");
	auto MetersSquared        = RegisterUnit(Model, "m2");
	auto InverseMetersSquared = RegisterUnit(Model, "1/m2");
	auto MetresCubedPerSecond = RegisterUnit(Model, "m3/s");
	auto MetersCubed          = RegisterUnit(Model, "m3");
	auto MetresPerSecond      = RegisterUnit(Model, "m/s");
	auto SecondsPerCubeRootM  = RegisterUnit(Model, "s/(m^1/3)");
	auto SquareKm             = RegisterUnit(Model, "km2");
	auto CubicMetersPerSecond = RegisterUnit(Model, "m3/s");
	auto MilligramsPerLiter   = RegisterUnit(Model, "mg/l");


	auto SolverPrecGroup = RegisterParameterGroup(Model, "Solver resolution");
	auto SolverPrec      = RegisterParameterDouble(Model, SolverPrecGroup, "Solver resolution", Dimensionless, 0.1, 0.001, 0.5, "Default should be 0.1, decrease this if the reach hydrology misbehaves. This should only be necessary if you have reaches shorter than about 1km");

	auto LandscapeUnits = RegisterIndexSet(Model, "Landscape units");
	auto Land = RegisterParameterGroup(Model, "Hydrology by land class", LandscapeUnits);
	
	auto SoilBoxes = RegisterIndexSet(Model, "Soils");
	
	auto SnowMultiplier              = RegisterParameterDouble(Model, Land, "Snow multiplier",             Dimensionless,     1.0,  0.5,    1.5, "Adjustment factor used to account for bias in the relationship between snow measured in the gauge and effective snowfall amounts falling", "Msnow");
	auto SnowMeltTemperature         = RegisterParameterDouble(Model, Land, "Snow melt temperature",       DegreesCelsius,    0.0, -4.0,    4.0, "The temperature at or above which snow can melt", "Tsnow");
	auto DegreeDayMeltFactor         = RegisterParameterDouble(Model, Land, "Degree day melt factor",      MmPerDegreePerDay, 3.0,  1.0,    4.0, "Describes the dependency of snow melt rates on temperature. The parameter represents the number of millimetres water melted per degree celcius above the snow melt temperature", "DDFmelt");
	auto RainMultiplier              = RegisterParameterDouble(Model, Land, "Rain multiplier",             Dimensionless,     1.0,  0.5,    1.5, "Adjustment factor used to account for bias in the relationship between rain measured in the gauge and effective rainfall amounts falling", "Mrain");
	auto InitialSnowDepth            = RegisterParameterDouble(Model, Land, "Initial snow depth",          MmSWE,             0.0,  0.0, 9999.0, "The depth of snow, expressed as water equivalents, at the start of the simulation");
	auto DegreeDayEvapotranspiration = RegisterParameterDouble(Model, Land, "Degree day evapotranspiration", MmPerDegreePerDay, 0.12, 0.05,   0.2, "Describes the baseline dependency of evapotranspiration on air temperature. The parameter represents the number of millimetres water evapotranspired per degree celcius above the growing degree threshold when evapotranspiration rates are not soil moisture limited", "DDET");
	auto GrowingDegreeThreshold      = RegisterParameterDouble(Model, Land, "Growing degree threshold",    DegreesCelsius,    0.0, -4.0,    4.0, "The temperature at or above which plant growth and hence evapotranspiration are assumed to occur", "Tgrow");
    auto CanopyInterception          = RegisterParameterDouble(Model, Land, "Canopy interception",         MmPerDay,          0.0,  0.0,    0.3, "The depth of precipitation which does not make it to the soil surface but is instead intercepted by the vegetative canopy and returned to the atmosphere either through evaporation or sublimation", "canop");

	
	auto SoilsLand = RegisterParameterGroup(Model, "Soil characteristics by land class", SoilBoxes, LandscapeUnits);

    auto InitialWaterDepth = RegisterParameterDouble(Model, SoilsLand, "Initial water depth", Mm, 200.0, 0.0, 9999.0, "The initial depth of water in a box at the start of a simulation");
	auto RelativeAreaIndex = RegisterParameterDouble(Model, SoilsLand, "Relative area index", Dimensionless, 1.0, 0.0, 1.0, "The areal fraction of the simulation covered by a box, typical INCA-type simulations will use a value of 1.0");
	auto Infiltration      = RegisterParameterDouble(Model, SoilsLand, "Infiltration", MmPerDay, 100.0, 0.0, 500.0, "The maximum rate at which water can infiltrate into a box from overlying boxes", "inf");
	auto RetainedWaterDepth = RegisterParameterDouble(Model, SoilsLand, "Retained water depth", Mm, 100.0, 0.0, 100000.0, "The depth of water in a box which does not contribute to runoff but can contribute to evapotranspiration and to diluting nutrient inputs to the box. For the soil water box, this is similar to the field capacity");
    auto DroughtRunoffFraction = RegisterParameterDouble(Model, SoilsLand, "Drought runoff fraction", Dimensionless, 0.0, 0.0, 0.5, "The fraction of water entering a box which contributes to runoff generation when the depth of water is below the retained water depth", "drought");
    auto TimeConstant = RegisterParameterDouble(Model, SoilsLand, "Time constant", Days, 5.0, 1.0, 9999.0, "The inverse of the rate at which water flows out of a box", "Tc");
	auto EvapotranspirationAdjustment = RegisterParameterDouble(Model, SoilsLand, "Evapotranspiration adjustment", Dimensionless, 1.0, 0.0, 10.0, "A factor to slow the rate of evapotranspiration when the depth of water in a box is below the retained water depth. Special  values include 0 (no slowing of evapotranspiration, 1 (slowing is proportional to the depth of water remaining in the bucket) and values above 10 (all evapotranspiration effectively stops when the depth of water is below the retained water depth)", "Aevap");
	auto RelativeEvapotranspirationIndex = RegisterParameterDouble(Model, SoilsLand, "Relative evapotranspiration index", Dimensionless, 1.0, 0.0, 1.0, "The fraction of the total evapotranspiration in a landscape unit which is to be generated from the current bucket");
	auto MaximumCapacity = RegisterParameterDouble(Model, SoilsLand, "Maximum capacity", Mm, 1000.0, 0.0, 9999.0, "The maximum depth of water which can be held in a bucket. For soil water, this is similar to the saturation capacity");

	auto BoxType = RegisterParameterGroup(Model, "Soil box type", SoilBoxes);
	
	auto ThisIsAQuickBox = RegisterParameterBool(Model, BoxType, "This is a quick box", true);

	auto Reach = RegisterIndexSetBranched(Model, "Reaches");
	auto Reaches = RegisterParameterGroup(Model, "Reach and subcatchment characteristics", Reach);


	auto TerrestrialCatchmentArea = RegisterParameterDouble(Model, Reaches, "Terrestrial catchment area", SquareKm, 500.0, 0.01, 999999.0, "The terrestrial area of a subcatchment, excluding open water");
	auto ReachLength              = RegisterParameterDouble(Model, Reaches, "Reach length", Meters, 1000.0, 1.0, 999999.0, "The length of the main stem of the stream / reach in a subcatchment");
	auto ReachBottomWidth         = RegisterParameterDouble(Model, Reaches, "Reach bottom width", Meters, 10.0, 0.1, 9999.0, "The bottom width of the main stem of the stream / reach in a subcatchment");
	auto ReachBankSlope           = RegisterParameterDouble(Model, Reaches, "Reach bank slope", Dimensionless, 0.3, 0.01, 10.0, "The slope of the river bank");
	auto ReachSlope               = RegisterParameterDouble(Model, Reaches, "Reach slope", Dimensionless, 0.01, 0.000001, 0.2, "Roughly the difference in elevation between the ends divided by the length");
	auto ManningsRoughness        = RegisterParameterDouble(Model, Reaches, "Manning's roughness coefficent", SecondsPerCubeRootM, 0.04, 0.01, 0.1, "The roughness coefficent n in Manning's flow velocity equation", "Cmann");
	auto SnowThresholdTemperature = RegisterParameterDouble(Model, Reaches, "Snow threshold temperature", DegreesCelsius, 0.0, -4.0, 4.0, "The temperature at or below which precipitation will fall as snow in a subcatchment");
	auto ReachSnowMultiplier = RegisterParameterDouble(Model, Reaches, "Reach snow multiplier", Dimensionless, 1.0, 0.5, 2.0, "The subcatchment-specific snow multiplier needed to account for possible spatial variability between the precipitation monitoring site and the subcatchment", "RMsnow");
	auto ReachRainMultiplier = RegisterParameterDouble(Model, Reaches, "Reach rain multiplier", Dimensionless, 1.0, 0.5, 2.0, "The subcatchment specific rain multiplier needed to account for possible spatial variability between the precipitation monitoring site and the subcatchment", "RMrain");
	auto AbstractionFlow = RegisterParameterDouble(Model, Reaches, "Abstraction flow", CubicMetersPerSecond, 0.0, 0.0, 9999.0, "The rate at which water is removed from a reach by human activities");
	auto EffluentFlow = RegisterParameterDouble(Model, Reaches, "Effluent flow", CubicMetersPerSecond, 0.0, 0.0, 9999.0, "The rate of liquid inputs to a reach from e.g. sewage treatment works");

	auto InitialStreamFlow = RegisterParameterDouble(Model, Reaches, "Initial stream flow", CubicMetersPerSecond, 0.1, 0.0001, 9999.0, "The flow in the stream at the start of the simulation. This parameter is only used for reaches that don't have any other reaches as inputs.");
	
	auto LandUsePercentages = RegisterParameterGroup(Model, "Land use percentages", Reach, LandscapeUnits);

    auto PercentU = RegisterUnit(Model, "%");

    auto Percent = RegisterParameterDouble(Model, LandUsePercentages, "%", PercentU, 25.0, 0.0, 100.0, "The percentage of a subcatchment occupied by a specific land cover type");

	auto Percolation = RegisterParameterGroup(Model, "Percolation", LandscapeUnits, SoilBoxes, SoilBoxes);
	auto PercolationMatrix = RegisterParameterDouble(Model, Percolation, "Percolation matrix", Dimensionless, 0.05, 0.0, 1.0, "The fraction of runoff that goes from one box to another", "perc");
	
	
	auto SnowFall = RegisterEquation(Model, "Snow fall", MmPerDay);
	auto SnowMelt = RegisterEquation(Model, "Snow melt", MmPerDay);
	auto RainFall = RegisterEquation(Model, "Rainfall",  MmPerDay);
	auto SnowAsWaterEquivalent = RegisterEquation(Model, "Snow depth as water equivalent", MmSWE);
	SetInitialValue(Model, SnowAsWaterEquivalent, InitialSnowDepth);
	
	
	auto ActualPrecipitation = RegisterInput(Model, "Actual precipitation", MmPerDay);
	auto AirTemperature      = RegisterInput(Model, "Air temperature", DegreesCelsius);
	
	
	EQUATION(Model, SnowFall,
		double snowfall = Max(PARAMETER(ReachSnowMultiplier) * PARAMETER(SnowMultiplier) * INPUT(ActualPrecipitation) - PARAMETER(CanopyInterception), 0.0);
		if(INPUT(AirTemperature) > PARAMETER(SnowThresholdTemperature)) snowfall = 0.0;
		return snowfall;
	)
	
	EQUATION(Model, SnowMelt,
		double melt = (INPUT(AirTemperature) - PARAMETER(SnowMeltTemperature)) * PARAMETER(DegreeDayMeltFactor);
		double snowmelt = Min(LAST_RESULT(SnowAsWaterEquivalent), melt);
		if(INPUT(AirTemperature) < PARAMETER(SnowMeltTemperature)) snowmelt = 0.0;
		return snowmelt;
	)
	
	EQUATION(Model, RainFall,
		double rainfall = Max(PARAMETER(ReachRainMultiplier) * PARAMETER(RainMultiplier) * INPUT(ActualPrecipitation) - PARAMETER(CanopyInterception), 0.0);
		if(INPUT(AirTemperature) <= PARAMETER(SnowThresholdTemperature)) rainfall = 0.0;
		return rainfall;
	)

	EQUATION(Model, SnowAsWaterEquivalent,
		return LAST_RESULT(SnowAsWaterEquivalent) + RESULT(SnowFall) - RESULT(SnowMelt);
	)

	auto PercolationInput   = RegisterEquation(Model, "Percolation input", MmPerDay);
	auto WaterInput         = RegisterEquation(Model, "Input", MmPerDay);
	auto WaterDepth1        = RegisterEquation(Model, "Water depth 1", Mm);
	auto SaturationExcess   = RegisterEquation(Model, "Saturation excess", MmPerDay);
	auto WaterDepth2        = RegisterEquation(Model, "Water depth 2", Mm);
	auto EvapoTranspirationX3 = RegisterEquation(Model, "Evapotranspiration X3", MmPerDay);
	auto EvapoTranspirationX4 = RegisterEquation(Model, "Evapotranspiration X4", MmPerDay);
	auto EvapoTranspiration   = RegisterEquation(Model, "Evapotranspiration", MmPerDay);
	auto WaterDepth3        = RegisterEquation(Model, "Water depth 3", Mm);
	auto TotalRunoff        = RegisterEquation(Model, "Total runoff", MmPerDay);
	auto DroughtRunoff      = RegisterEquation(Model, "Drought runoff", MmPerDay);
	auto PercolationOut     = RegisterEquation(Model, "Percolation out", MmPerDay);
	auto SaturationExcessInput = RegisterEquation(Model, "Saturation excess input", MmPerDay);
	auto WaterDepth4        = RegisterEquation(Model, "Water depth 4", Mm);
	auto Runoff             = RegisterEquation(Model, "Runoff", MmPerDay);
	auto RunoffToReach      = RegisterEquation(Model, "Runoff to reach", MmPerDay);
	auto WaterDepth         = RegisterEquation(Model, "Water depth", Mm);
	SetInitialValue(Model, WaterDepth, InitialWaterDepth);
	
	auto TotalRunoffToReach = RegisterEquationCumulative(Model, "Total runoff to reach", RunoffToReach, SoilBoxes);
	
	EQUATION(Model, PercolationInput,
		//NOTE : The value of this state variable, instead of being computed by this equation, is set by earlier boxes' PercolationOut computation. See below.
	
		LAST_RESULT(WaterDepth2); //NOTE: To force index set dependencies
		
		return RESULT(PercolationInput, CURRENT_INDEX(SoilBoxes)); //NOTE: Hack to make it not overwrite the value that is set in PercolationOut.
	)
    
	EQUATION(Model, WaterInput,
		double input = RESULT(PercolationInput);
		double input2 = RESULT(SnowMelt) + RESULT(RainFall);
		if(PARAMETER(ThisIsAQuickBox)) input += input2;
		return input;
	)

	EQUATION(Model, WaterDepth1,
		return LAST_RESULT(WaterDepth) + RESULT(WaterInput);
	)

	EQUATION(Model, SaturationExcess,
		double saturationExcess = Max(0.0, RESULT(WaterDepth1) - PARAMETER(MaximumCapacity));
		if(PARAMETER(ThisIsAQuickBox)) saturationExcess = 0.0;
		return saturationExcess;
	)

	EQUATION(Model, WaterDepth2,
		return RESULT(WaterDepth1) - RESULT(SaturationExcess);
	)
	
	EQUATION(Model, EvapoTranspirationX3,
		return Max(0.0, INPUT(AirTemperature) - PARAMETER(GrowingDegreeThreshold));
	)

	EQUATION(Model, EvapoTranspirationX4,
		double evapotranspirationadjustment = PARAMETER(EvapotranspirationAdjustment);
		if(RESULT(WaterDepth2) < PARAMETER(RetainedWaterDepth))
			return pow(RESULT(WaterDepth2) / PARAMETER(RetainedWaterDepth), evapotranspirationadjustment);
		return 1.0;
	)

	EQUATION(Model, EvapoTranspiration,
		return Min(RESULT(WaterDepth2),
			RESULT(EvapoTranspirationX3) * RESULT(EvapoTranspirationX4) * PARAMETER(RelativeEvapotranspirationIndex) * PARAMETER(DegreeDayEvapotranspiration));
	)
	
    EQUATION(Model, WaterDepth3,
		return RESULT(WaterDepth2) - RESULT(EvapoTranspiration);
	)

	EQUATION(Model, TotalRunoff,
		double totalrunoff = (RESULT(WaterDepth3) - PARAMETER(RetainedWaterDepth)) / PARAMETER(TimeConstant);
		double droughtrunoffraction = PARAMETER(DroughtRunoffFraction);
		double waterinput = RESULT(WaterInput);
		if(RESULT(WaterDepth3) < PARAMETER(RetainedWaterDepth))
		{
			totalrunoff = waterinput * droughtrunoffraction;
		}
		return totalrunoff;
	)

	EQUATION(Model, DroughtRunoff,
		double droughtrunoff = 0.0;
		if(RESULT(WaterDepth3) < PARAMETER(RetainedWaterDepth))
		{
			droughtrunoff = RESULT(TotalRunoff);
		}
		return droughtrunoff;
	)

    EQUATION(Model, PercolationOut,
		double percolationOut = 0.0;
		double relativeareaindex = PARAMETER(RelativeAreaIndex);
		double totalrunoff = RESULT(TotalRunoff);
		
		index_t LU = CURRENT_INDEX(LandscapeUnits);
		
		for(index_t OtherSoil = CURRENT_INDEX(SoilBoxes) + 1; OtherSoil < INDEX_COUNT(SoilBoxes); ++OtherSoil)
		{
			//NOTE: (mdn 21.10.2019)  Here we should only check if we exceed the infiltration limit of the receiving box. We SHOULD overflow the saturation capacity (i.e. "Maximum capacity" parameter) of the receiving box. The receiving box then checks in its own SaturationExcess equation if it is overflowing and routs any saturation excess to the quick box.
			
			//NOTE: Infiltration limit of receiving box
			double mpi = PARAMETER(Infiltration, OtherSoil, LU);
			
			double perc = Min(mpi * PARAMETER(RelativeAreaIndex, OtherSoil, LU) / relativeareaindex,
				PARAMETER(PercolationMatrix, OtherSoil) * totalrunoff);
			
			//NOTE: Don't percolate more water than we have left (though this should not happen... TODO: find out if this check is really necessary)
			perc = Min(perc, RESULT(WaterDepth3) - percolationOut);
			
			percolationOut += perc;
			
			//TODO: Setting the result of another equation from this equation is a bad way to do it.... It obfuscates the execution order of equations.
			// We should find a way to do it "properly" instead
			double percinput = RESULT(PercolationInput, OtherSoil) + perc * PARAMETER(RelativeAreaIndex) / PARAMETER(RelativeAreaIndex, OtherSoil, LU);
			SET_RESULT(PercolationInput, percinput, OtherSoil);
		}
		return percolationOut;
	)
	
	EQUATION(Model, SaturationExcessInput,
		double sumSaturationExcessInput = 0.0;
		double relativeareaindex = PARAMETER(RelativeAreaIndex);
		for(index_t OtherSoil = FIRST_INDEX(SoilBoxes); OtherSoil < INDEX_COUNT(SoilBoxes); ++OtherSoil)
		{
			sumSaturationExcessInput += LAST_RESULT(SaturationExcess, OtherSoil)
							* PARAMETER(PercolationMatrix, OtherSoil, CURRENT_INDEX(SoilBoxes))
							* PARAMETER(RelativeAreaIndex, OtherSoil, CURRENT_INDEX(LandscapeUnits))
							/ relativeareaindex;
		}
		if(!PARAMETER(ThisIsAQuickBox)) sumSaturationExcessInput = 0.0;
		return sumSaturationExcessInput;
	)

	/*
	Mdn (21.10.2019)
	New change in version 1.3: We now add saturation excess input (something that is received by quick boxes) after percolation is determined. This is because otherwise water would move back and forth between the quick box and any overflowing soil boxes. We then make it so that saturation excess is always delivered to the stream (at least when retention time in the quick box is <= 1).

	Note that the computation of saturation excess runoff (i.e. the fraction of saturation excess input that then runs off to the reach) is a little edgy, but it will work unless somebody has a very strange parametrization of the model.

	To make this all work more fluidly (pun intended), one would have to make this all into an ODE system, but it seems to work OK like it is now.
	*/

	EQUATION(Model, WaterDepth4,
		return RESULT(WaterDepth3) - RESULT(PercolationOut) + RESULT(SaturationExcessInput);
	)

	EQUATION(Model, Runoff,
		double satexrunoff = Min(RESULT(SaturationExcessInput) / PARAMETER(TimeConstant), RESULT(SaturationExcessInput)); //NOTE in case tc < 1
	
		double runoff = RESULT(TotalRunoff) - RESULT(PercolationOut) + satexrunoff;
		
		runoff = Min(runoff, RESULT(WaterDepth4));
		/*
		//Mdn (21.10.2019): I don't see the purpose of the following code. We always have runoff==runoff2 unless there is infiltration excess. But infiltration excess should (as specified in the article) always go to the reach regardless of whether the box is a quick box or a regular box.
		
		double runoff2 = Min(runoff, PARAMETER(PercolationMatrix) * RESULT(TotalRunoff));
		if(!PARAMETER(ThisIsAQuickBox))
		{
			runoff = runoff2;
		}
		*/
		return runoff;
	)
	
	EQUATION(Model, RunoffToReach,
		return RESULT(Runoff) * PARAMETER(RelativeAreaIndex);
	)

    EQUATION(Model, WaterDepth,
		return RESULT(WaterDepth4) - RESULT(Runoff);
	)

    
	auto AbstractionTimeseries = RegisterInput(Model, "Abstraction flow", MetresCubedPerSecond);
	auto EffluentTimeseries    = RegisterInput(Model, "Effluent flow", MetresCubedPerSecond);

	auto ReachSolver = RegisterSolver(Model, "Reach solver", SolverPrec, IncaDascru);
	
	auto DiffuseFlowOutput = RegisterEquation(Model, "Diffuse flow output", MetresCubedPerSecond);
	auto ReachFlowInputUpstream = RegisterEquation(Model, "Flow input from upstream", MetresCubedPerSecond);
	auto ReachFlowInput    = RegisterEquation(Model, "Reach flow input", MetresCubedPerSecond);
	auto ReachAbstraction  = RegisterEquation(Model, "Reach abstraction", MetresCubedPerSecond, ReachSolver);
	
	auto TotalDiffuseFlowOutput = RegisterEquationCumulative(Model, "Total diffuse flow output", DiffuseFlowOutput, LandscapeUnits);
	
	auto ReachCrossSectionArea    = RegisterEquation(Model, "Reach cross section area", MetersSquared, ReachSolver);
	auto ReachDepth               = RegisterEquation(Model, "Reach depth", Meters, ReachSolver);
	auto ReachTopWidth            = RegisterEquation(Model, "Reach top width", Meters, ReachSolver);
	auto ReachWettedPerimeter     = RegisterEquation(Model, "Reach wetted perimeter", Meters, ReachSolver);
	auto ReachHydraulicRadius     = RegisterEquation(Model, "Reach hydraulic radius", Meters, ReachSolver);
	auto ReachVelocity            = RegisterEquation(Model, "Reach velocity", MetresPerSecond, ReachSolver);
	
	auto InitialReachFlow         = RegisterEquationInitialValue(Model, "Initial reach flow", MetresCubedPerSecond);
	auto ReachFlow                = RegisterEquation(Model, "Reach flow", MetresCubedPerSecond, ReachSolver);
	SetInitialValue(Model, ReachFlow, InitialReachFlow);
	
	auto InitialReachVolume       = RegisterEquationInitialValue(Model, "Initial reach volume", MetersCubed);
	auto ReachVolume              = RegisterEquationODE(Model, "Reach volume", MetersCubed, ReachSolver);
	SetInitialValue(Model, ReachVolume, InitialReachVolume);
	
	auto DailyMeanReachFlow       = RegisterEquationODE(Model, "Reach flow (daily mean)", MetresCubedPerSecond, ReachSolver);
	ResetEveryTimestep(Model, DailyMeanReachFlow);
	
	
	// Stream flow (m3/s) = Catchment Area (km2)
	//                      * Landscape unit proportion (% / 100)
	//                      * Runoff (mm/d)
	//                      * 1e6 m2/km2 * 1e-3 m/mm * 1/86400 d/s
	EQUATION(Model, DiffuseFlowOutput,
		return (PARAMETER(TerrestrialCatchmentArea) * (PARAMETER(Percent) / 100.0) * RESULT(TotalRunoffToReach) * 1000000.0 * 0.001 * (1.0 / 86400.0));
	)
	
	EQUATION(Model, ReachFlowInputUpstream,
		double input = 0.0;
		
		for(index_t Input : BRANCH_INPUTS(Reach))
			input += RESULT(DailyMeanReachFlow, Input);

		return input;	
	)

	EQUATION(Model, ReachFlowInput,
		return RESULT(ReachFlowInputUpstream) + RESULT(TotalDiffuseFlowOutput) + IF_INPUT_ELSE_PARAMETER(EffluentTimeseries, EffluentFlow);
	)
    
	EQUATION(Model, InitialReachFlow,
		double upstreamFlow = 0.0;
		for(index_t Input : BRANCH_INPUTS(Reach))
			upstreamFlow += RESULT(ReachFlow, Input);
		
		double initialFlow  = PARAMETER(InitialStreamFlow);
		
		return (INPUT_COUNT(Reach) == 0) ? initialFlow : upstreamFlow;
	)
	
	EQUATION(Model, ReachCrossSectionArea,
		return RESULT(ReachVolume) / PARAMETER(ReachLength);
	)
	
	EQUATION(Model, ReachDepth,
		double b  = PARAMETER(ReachBankSlope);
		double WB = PARAMETER(ReachBottomWidth);
		double A  = RESULT(ReachCrossSectionArea);
		double WBb = WB*b;
		return 0.5 * (sqrt(WBb*WBb + 4.0*A*b) - WBb);
	)
	
	EQUATION(Model, ReachTopWidth,
		return PARAMETER(ReachBottomWidth) + 2.0 * RESULT(ReachDepth) / PARAMETER(ReachBankSlope);
	)
	
	EQUATION(Model, ReachWettedPerimeter,
		double b = PARAMETER(ReachBankSlope);
		return PARAMETER(ReachBottomWidth) + 2.0*RESULT(ReachDepth)*sqrt(1.0 + 1.0/(b*b));
	)
	
	EQUATION(Model, ReachHydraulicRadius,
		return RESULT(ReachCrossSectionArea) / RESULT(ReachWettedPerimeter);
	)
	
	EQUATION(Model, ReachVelocity,
		double R = RESULT(ReachHydraulicRadius);
		double S = PARAMETER(ReachSlope);
		double n = PARAMETER(ManningsRoughness);
		return pow(R, 2.0/3.0)*sqrt(S)/n;
	)
	
	EQUATION(Model, ReachFlow,
		return RESULT(ReachCrossSectionArea) * RESULT(ReachVelocity);
	)
	
	EQUATION(Model, ReachAbstraction,
		double abstraction = IF_INPUT_ELSE_PARAMETER(AbstractionTimeseries, AbstractionFlow);
		
		//TODO: This is not really a good comparison as we could allow abstraction if there is enough effluent + other inputs
		return Min(abstraction, RESULT(ReachVolume)/86400.0);
	)
	
	EQUATION(Model, InitialReachVolume,
		//NOTE: This is not entirely correct, and so will not cause the right initial reach flow.. :( Assumes rectangular instead of trapezoid cross-section. But will not be very far off, and it will correct itself within a couple of days.
		
		return PARAMETER(ReachLength)*pow(RESULT(ReachFlow)*PARAMETER(ManningsRoughness)*pow(PARAMETER(ReachBottomWidth), 2.0/3.0)/sqrt(PARAMETER(ReachSlope)), 3.0/5.0);
	)
	
	EQUATION(Model, ReachVolume,
		return (RESULT(ReachFlowInput) - RESULT(ReachAbstraction) - RESULT(ReachFlow)) * 86400.0;
	)
	
	EQUATION(Model, DailyMeanReachFlow,
		return RESULT(ReachFlow);
	)
	
	EndModule(Model);
}

#define PERSIST_MODEL_H
#endif