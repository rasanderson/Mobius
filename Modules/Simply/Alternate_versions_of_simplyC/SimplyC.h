#include "../UnitConversions.h"

static void
AddSimplyCModel(mobius_model *Model)
{
	BeginModule(Model, "SimplyC", "0.0.0.0.0.235");
	
	// Inputs
	auto AirTemperature = GetInputHandle(Model, "Air temperature");

	// Solvers already defined in hydrology module
	auto LandSolver = GetSolverHandle(Model, "SimplyQ land solver");
	auto ReachSolver = GetSolverHandle(Model, "SimplyQ reach solver");

	// Units
	auto Kg			= RegisterUnit(Model, "kg");
	auto KgPerDay	= RegisterUnit(Model, "kg/day");
	auto Dimensionless = RegisterUnit(Model);
	auto MgPerL		 = RegisterUnit(Model, "mg/l");
	auto PerDegreesC = RegisterUnit(Model, "1/degreesC");

	// Set up index sets
	auto Reach          = GetIndexSetHandle(Model, "Reaches"); //Defined in SimplyQ.h
	auto LandscapeUnits = GetIndexSetHandle(Model, "Landscape units"); //Defined in SimplyQ.h
//	auto LowCarbon   = RequireIndex(Model, LandscapeUnits, "Low soil carbon"); //Not sure whether to do this or just recommend people follow this
//	auto HighCarbon  = RequireIndex(Model, LandscapeUnits, "High soil carbon"); //Not sure whether to do this

	// PARAMS
	
	// Params defined in hydrol model
	auto CatchmentArea               = GetParameterDoubleHandle(Model, "Catchment area");
#ifdef SIMPLYQ_GROUNDWATER
	auto BaseflowIndex               = GetParameterDoubleHandle(Model, "Baseflow index");
#endif
	auto LandUseProportions			 = GetParameterDoubleHandle(Model, "Land use proportions");
	auto ProportionToQuickFlow		 = GetParameterDoubleHandle(Model, "Proportion of precipitation that contributes to quick flow");

	// Carbon params that don't vary with land class or sub-catchment/reach
	auto CarbonParamsGlobal = RegisterParameterGroup(Model, "Carbon global");
	auto SoilTemperatureCoefficient = RegisterParameterDouble(Model, CarbonParamsGlobal, "Gradient of the soil water [DOC] response to changing soil temperature", PerDegreesC, 0.1, 0.001, 1.0);
	auto SoilDOCInterceptCoefficient = RegisterParameterDouble(Model, CarbonParamsGlobal, "Coefficient describing intercept in [DOC]= m * soilT + c equation, as a proportion of the baseline DOC concentration", Dimensionless, 0.5);
#ifdef SIMPLYQ_GROUNDWATER
	auto DeepSoilDOCConcentration = RegisterParameterDouble(Model, CarbonParamsGlobal, "Mineral soil/groundwater DOC concentration", MgPerL, 0.0, 0.0, 30.0);
#endif

	// Carbon params that vary with land class
	auto CarbonParamsLand = RegisterParameterGroup(Model, "Carbon land", LandscapeUnits);
	auto BaselineSoilDOCConcentration = RegisterParameterDouble(Model, CarbonParamsLand, "Baseline soil water DOC concentration", MgPerL, 10.0, 0.0, 70.0);

	// EQUATIONS

	// Equations defined in hydrology module required here
	auto SoilWaterVolume 			 = GetEquationHandle(Model, "Soil water volume");
	auto QuickFlow                   = GetEquationHandle(Model, "Quick flow");
	auto SoilWaterFlow   		     = GetEquationHandle(Model, "Soil water flow");
#ifdef SIMPLYQ_GROUNDWATER
	auto GroundwaterFlow             = GetEquationHandle(Model, "Groundwater flow");
#endif
	auto ReachVolume                 = GetEquationHandle(Model, "Reach volume");
	auto ReachFlow                   = GetEquationHandle(Model, "Reach flow (end-of-day)");
	auto DailyMeanReachFlow          = GetEquationHandle(Model, "Reach flow (daily mean, cumecs)");
	auto SnowMelt					 = GetEquationHandle(Model, "Snow melt");
	auto SnowDepth					 = GetEquationHandle(Model, "Snow depth as water equivalent");
	auto PrecipitationFallingAsRain  = GetEquationHandle(Model, "Precipitation falling as rain");

	// Equation from soil temperature module
	auto SoilTemperature       = GetEquationHandle(Model, "Soil temperature corrected for insulating effect of snow");

	// Carbon equations which vary by land class

	auto SoilWaterCarbonConcentration = RegisterEquation(Model, "Soil water DOC concentration, mg/l", MgPerL);
	
	auto QuickFlowCarbonFluxToReach = RegisterEquation(Model, "Quick flow DOC flux scaled by land class area", KgPerDay);
	
	auto SoilwaterCarbonFlux = RegisterEquation(Model, "Soil water carbon flux", KgPerDay);
	SetSolver(Model, SoilwaterCarbonFlux, LandSolver);
	
	auto DailyMeanSoilwaterCarbonFluxToReach = RegisterEquationODE(Model, "Soil water carbon flux to reach, daily mean", KgPerDay);
	SetInitialValue(Model, DailyMeanSoilwaterCarbonFluxToReach, 0.0);	
	SetSolver(Model, DailyMeanSoilwaterCarbonFluxToReach, LandSolver);
	ResetEveryTimestep(Model, DailyMeanSoilwaterCarbonFluxToReach);

	//TO DO: test whether get better results by raising soil temp to power 2 or not (empirical analysis from Langtjern surface water DOC)
	EQUATION(Model, SoilWaterCarbonConcentration,
		/* // Linear relationship between soil DOC concentration and soil temperature
		double minSoilTemp = -PARAMETER(SoilDOCInterceptCoefficient)/PARAMETER(SoilTemperatureCoefficient);
		double SoilTempAboveZero = Max(minSoilTemp, RESULT(SoilTemperature)); //Quick fix to prevent high soil concs with negative soil temp
		return PARAMETER(SoilTemperatureCoefficient) * PARAMETER(BaselineSoilDOCConcentration) * pow(SoilTempAboveZero,2)
			   + PARAMETER(SoilDOCInterceptCoefficient) * PARAMETER(BaselineSoilDOCConcentration);
		*/
		// Power law relationship between soil DOC concentration and soil temperature
		double SoilTempAboveZero = Max(0.0, RESULT(SoilTemperature));
		return PARAMETER(SoilTemperatureCoefficient) * PARAMETER(BaselineSoilDOCConcentration) * pow(SoilTempAboveZero, 2.0)
			   + PARAMETER(SoilDOCInterceptCoefficient) * PARAMETER(BaselineSoilDOCConcentration);
	)
	
/* 	EQUATION(Model, QuickFlowCarbonFluxToReach,
		return PARAMETER(LandUseProportions) * RESULT(QuickFlow)
			   * ConvertMgPerLToKgPerMm(RESULT(SoilWaterCarbonConcentration), PARAMETER(CatchmentArea));
	) */
	
	EQUATION(Model, QuickFlowCarbonFluxToReach,
		double quickDOCconcentration;
		double f_melt = PARAMETER(ProportionToQuickFlow)*RESULT(SnowMelt)/RESULT(QuickFlow);
		double f_rain = 1.0-f_melt;
		double soilwaterDOCconc = RESULT(SoilWaterCarbonConcentration);
		if (RESULT(QuickFlow)>0.) quickDOCconcentration = f_melt*6.0 + f_rain*soilwaterDOCconc;
		else quickDOCconcentration = soilwaterDOCconc;
			
		return RESULT(QuickFlow)
			   * ConvertMgPerLToKgPerMm(quickDOCconcentration, PARAMETER(CatchmentArea));
	)

	EQUATION(Model, SoilwaterCarbonFlux,
		return
			ConvertMgPerLToKgPerMm(RESULT(SoilWaterCarbonConcentration), PARAMETER(CatchmentArea))
#ifdef SIMPLYQ_GROUNDWATER
			* (1.0-PARAMETER(BaseflowIndex))
#endif
			* RESULT(SoilWaterFlow); // No groundwater
	)
	
	EQUATION(Model, DailyMeanSoilwaterCarbonFluxToReach,
		return RESULT(SoilwaterCarbonFlux);
	)
	
	// Instream equations

	auto TotalSoilwaterCarbonFluxToReach = RegisterEquationCumulative(Model, "Soilwater carbon flux to reach summed over landscape units", DailyMeanSoilwaterCarbonFluxToReach, LandscapeUnits, LandUseProportions);
	
	auto TotalQuickFlowCarbonFlux = RegisterEquationCumulative(Model, "Quick flow DOC flux to reach summed over landscape units", QuickFlowCarbonFluxToReach, LandscapeUnits, LandUseProportions);

#ifdef SIMPLYQ_GROUNDWATER	
	auto GroundwaterFluxToReach = RegisterEquation(Model, "Groundwater carbon flux to reach", KgPerDay);
	SetSolver(Model, GroundwaterFluxToReach, ReachSolver);
#endif
	
	//To do: work initial condition out from baseline DOC parameter
	auto StreamDOCMass = RegisterEquationODE(Model, "Reach DOC mass", Kg);
	SetInitialValue(Model, StreamDOCMass, 0.02); 
	SetSolver(Model, StreamDOCMass, ReachSolver);

	auto StreamDOCFluxOut = RegisterEquation(Model, "DOC flux from reach, end-of-day", KgPerDay);
	SetSolver(Model, StreamDOCFluxOut, ReachSolver);

	auto DailyMeanStreamDOCFlux = RegisterEquationODE(Model, "DOC flux from reach, daily mean", KgPerDay);
	SetInitialValue(Model, DailyMeanStreamDOCFlux, 0.0);
	SetSolver(Model, DailyMeanStreamDOCFlux, ReachSolver);
	ResetEveryTimestep(Model, DailyMeanStreamDOCFlux);

	auto ReachDOCConcentration = RegisterEquation(Model, "Reach DOC concentration (volume weighted daily mean)", MgPerL);

#ifdef SIMPLYQ_GROUNDWATER
 	EQUATION(Model, GroundwaterFluxToReach,
		return RESULT(GroundwaterFlow)*ConvertMgPerLToKgPerMm(PARAMETER(DeepSoilDOCConcentration), PARAMETER(CatchmentArea));
	)
#endif
	
	EQUATION(Model, StreamDOCMass,
		double upstreamflux = 0.0;
		for(index_t Input : BRANCH_INPUTS(Reach))
			upstreamflux += RESULT(DailyMeanStreamDOCFlux, Input);
		
		return
			RESULT(TotalQuickFlowCarbonFlux)
			+ RESULT(TotalSoilwaterCarbonFluxToReach)
#ifdef SIMPLYQ_GROUNDWATER
			+ RESULT(GroundwaterFluxToReach)
#endif
			+ upstreamflux
			- RESULT(StreamDOCFluxOut);		
	)
		
	EQUATION(Model, StreamDOCFluxOut,
		return 86400.0 * RESULT(ReachFlow) * SafeDivide(RESULT(StreamDOCMass), RESULT(ReachVolume));
	)

	EQUATION(Model, DailyMeanStreamDOCFlux,
		return RESULT(StreamDOCFluxOut);
	)

	EQUATION(Model, ReachDOCConcentration,
		return 1000.0 * SafeDivide(RESULT(StreamDOCMass), RESULT(ReachVolume));
	)
	
	EndModule(Model);
}

