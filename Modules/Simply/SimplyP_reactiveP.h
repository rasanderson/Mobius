

//NOTE: Only include these if you are going to use them (they cause long compile times):
//#include "../boost_solvers.h"
//#include "../mtl_solvers.h"


static void
AddSimplyPModel(mobius_model *Model)
{
	BeginModule(Model, "SimplyP", "0.4.3 reactiveP");
	
	SetModuleDescription(Model, R""""(
SimplyP is a parsimonious phosphorus model. It was originally implemented in Python and published as

[Jackson-Blake LA, Sample JE, Wade AJ, Helliwell RC, Skeffington RA. 2017. Are our dynamic water quality models too complex? A comparison of a new parsimonious phosphorus model, SimplyP, and INCA-P. Water Resources Research, 53, 5382–5399. doi:10.1002/2016WR020132](https://doi.org/10.1002/2016WR020132)

For news, updates and references, see [the model's github home page](https://github.com/NIVANorge/Mobius/tree/master/Applications/SimplyP)

New to version 0.4.3: Added TRP flux

New to version "reactiveP":
Added in extra P fractions to enable simulation of total reactive P (TRP), as this P fraction is commonly measured by automatic sensors at high frequency.
To do this, total particulate P (PP) is split into particulate organic P (POP) and particulate inorganic P, which is assumed to be particulate reactive P (PRP). The split is controlled by a static user-defined coefficient, in the same way that SRP is calculated from TDP. TRP is then SRP + PRP.
This model version was originally developed for use by Teagasc for modelling sites in the Irish Agricultural Catchment Programme.

New to version 0.4:
Landscape units are dynamic and user-specified instead of hardcoded.
Sediment and hydrology equations are factored out into separate modules (SimplyQ, SimplySed)

New to version 0.3:
More realistic hydrology.

[Detailed development log](https://github.com/NIVANorge/Mobius/blob/master/Applications/SimplyP/SimplyP_development_log.txt)

For reference, here is [the original Python implementation of SimplyP](https://github.com/LeahJB/SimplyP), which is no longer being developed.
)"""");
	
	// UNITS
	auto Dimensionless  = RegisterUnit(Model);
	auto Kg             = RegisterUnit(Model, "kg");
	auto Mm             = RegisterUnit(Model, "mm");
	auto MmPerKg        = RegisterUnit(Model, "mm/kg");
	auto KgPerM2        = RegisterUnit(Model, "kg/m^2");
	auto MgPerL         = RegisterUnit(Model, "mg/l");
	auto LPerMg         = RegisterUnit(Model, "l/mg");
	auto MgPerKg        = RegisterUnit(Model, "mg/kg");
	auto KgPerHaPerYear = RegisterUnit(Model, "kg/ha/year");
	auto KgPerDay       = RegisterUnit(Model, "kg/day");
	auto KgPerMm        = RegisterUnit(Model, "kg/mm");
	auto KgPerKm2       = RegisterUnit(Model, "kg/km^2");
	auto KgPerKm2PerDay = RegisterUnit(Model, "kg/km^2/day");
	
	// INDEX SETS
	auto Reach          = GetIndexSetHandle(Model, "Reaches");
	auto LandscapeUnits = GetIndexSetHandle(Model, "Landscape units");
	//auto Arable             = RequireIndex(Model, LandscapeUnits, "Arable");
	//auto ImprovedGrassland  = RequireIndex(Model, LandscapeUnits, "Improved grassland");
	//auto Seminatural        = RequireIndex(Model, LandscapeUnits, "Semi-natural");
	
	// PARAMETERS
	
	// Phosphorus parameters that don't vary by sub-catchment/reach or land class
	auto Phosphorous = RegisterParameterGroup(Model, "Phosphorous");
	
	auto DynamicEPC0                    = RegisterParameterBool(Model, Phosphorous, "Dynamic soil water EPC0, TDP and soil labile P", true, "Calculate a dynamic soil water EPC0 (the equilibrium P concentration of zero sorption), and therefore soilwater TDP concentration, so that it varies with labile P content? The labile P will also therefore vary");
	auto CalibrationMode                = RegisterParameterBool(Model, Phosphorous, "Run in calibration mode", true, "Run model in calibration mode? If true, the initial agricultural soil water TDP concentration (and therefore EPC0) is calibrated and used to estimate the phosphorus sorption coefficient. If false, the sorption coefficient is read in from the parameter file");
	
	auto MSoilPerM2                     = RegisterParameterDouble(Model, Phosphorous, "Soil mass per m2", KgPerM2, 95.0, 0.0, 200.0, "Estimate as topsoil depth * bulk density. Used to estimate the initial mass of labile soil P. Mostly important if you are interested in longer-term changes in soil P content", "Msoil");
	auto PhosphorousSorptionCoefficient = RegisterParameterDouble(Model, Phosphorous, "Phosphorous sorption coefficient", LPerMg, 1.13e-4, 0.0, 0.1, "Gradient of linear relationship between labile P and TDP concentration. This value is only used if calibration run mode is set to false, otherwise it is estimated by the model", "Ks");
	
	auto PPEnrichmentFactor             = RegisterParameterDouble(Model, Phosphorous, "Particulate P enrichment factor", Dimensionless, 1.6, 1.0, 5.0, "P content of eroded sediment compared to P content of bulk soils (multiplicative factor)", "EPP");
	auto SRPFraction                    = RegisterParameterDouble(Model, Phosphorous, "SRP fraction", Dimensionless, 0.7, 0.0, 1.0, "Factor to multiply TDP by to estimate instream SRP concentration", "fSRP");
	auto PRPFraction                    = RegisterParameterDouble(Model, Phosphorous, "PRP fraction", Dimensionless, 0.4, 0.0, 1.0, "Factor to multiply total particulate P by to estimate instream particulate reactive P (PRP) concentration", "fPRP");

	// Phosphorus parameters that vary by sub-catchment/reach
	auto PhosphorousReach = RegisterParameterGroup(Model, "Phosphorous reach", Reach);
	
	auto GroundwaterTDPConcentration    = RegisterParameterDouble(Model, PhosphorousReach, "Groundwater TDP concentration", MgPerL, 0.02, 0.0, 10.0, "Groundwater TDP concentration is constant through the model run. For small catchments, we recommend keeping this constant across sub-catchments", "TDPgw");
	auto EffluentTDP                    = RegisterParameterDouble(Model, PhosphorousReach, "Reach effluent TDP inputs", KgPerDay, 0.1, 0.0, 10.0, "These are EFFECTIVE inputs, i.e. immediate instream removal proportion * raw measured inputs. We do not include the removal fraction as a separate parameter as it would be linearly correlated with the sewage input parameter in uncertainty analysis", "TDPeff");
	
	// Phorphorus parameters that vary by land class
	auto PhosphorousLand = RegisterParameterGroup(Model, "Phosphorous land", LandscapeUnits);
	auto NetAnnualPInput                = RegisterParameterDouble(Model, PhosphorousLand, "Net annual P input to soil", KgPerHaPerYear, 10.0, -100.0, 100.0, "Net annual soil P budget (fertilizer + manure - harvest removal","P_in");
	auto InitialEPC0                    = RegisterParameterDouble(Model, PhosphorousLand, "Initial soil water TDP concentration and EPC0", MgPerL, 0.1, 0.0, 10.0, "Recommend setting this to 0 for semi-natural (or low soil P) land, and just providing a value for agricultural (or high soil P land). If the dynamic soil P option is set to false, this value is the soil water TDP concentration throughout the model run", "TDPsw");
	auto InitialSoilPConcentration      = RegisterParameterDouble(Model, PhosphorousLand, "Initial total soil P content", MgPerKg, 1458, 0.0, 10000.0, "", "TotalSoilP_init");
	auto SoilInactivePConcentration     = RegisterParameterDouble(Model, PhosphorousLand, "Inactive soil P content", MgPerKg, 873, 0.0, 10000.0, "For both semi-natural (or low-P) and agricultural (high-P) land, we recommend using the initial total soil P content of semi-natural (or low-P land), unless the soil types are very different", "InactiveP");

	
	// Optional timeseries that the user can use instead of the corresponding parameters.
	auto NetAnnualPInputTimeseries = RegisterInput(Model, "Net annual P input to soil", KgPerHaPerYear);
	auto EffluentTDPTimeseries = RegisterInput(Model, "Effluent TDP inputs", KgPerDay);
	
	
	// Params defined in hydrol or sed modules
	auto CatchmentArea               = GetParameterDoubleHandle(Model, "Catchment area");
	auto SedimentInputNonlinearCoefficient = GetParameterDoubleHandle(Model, "Sediment input non-linear coefficient");
	auto LandUseProportions          = GetParameterDoubleHandle(Model, "Land use proportions");
	auto BaseflowIndex               = GetParameterDoubleHandle(Model, "Baseflow index");
	
	
	// START EQUATIONS
	
	// Equations defined in other modules
	auto SoilWaterVolume                = GetEquationHandle(Model, "Soil water volume");
	auto QuickFlow                      = GetEquationHandle(Model, "Quick flow");
	auto SoilWaterFlow                  = GetEquationHandle(Model, "Soil water flow");	
	auto ReachVolume                    = GetEquationHandle(Model, "Reach volume");
	auto ReachFlow                      = GetEquationHandle(Model, "Reach flow (end-of-day)");
	auto DailyMeanReachFlow             = GetEquationHandle(Model, "Reach flow (daily mean, cumecs)");
	auto GroundwaterFlow                = GetEquationHandle(Model, "Groundwater flow");
	auto ReachSedimentInputCoefficient  = GetEquationHandle(Model, "Sediment input coefficient");
	auto ErosionFactor                  = GetEquationHandle(Model, "Erosion factor");
	
	
 	// P sorption coefficient calculation
	
	
	//TODO: It makes more sense to the units if Kf, the sorption coefficient, is in l/(mg*day) since  Kf*Msoil*(TDP/V-EPC0) is supposed to be a flux..
	auto SoilPSorptionCoefficient = RegisterEquationInitialValue(Model, "Soil phosphorous sorption coefficient", LPerMg);
	ParameterIsComputedBy(Model, PhosphorousSorptionCoefficient, SoilPSorptionCoefficient, false); //NOTE: The 'false' means that the parameter is nevertheless exposed in MobiView and the parameter file.
	
	EQUATION(Model, SoilPSorptionCoefficient,
	
		double Kf = 1e-6 * SafeDivide((PARAMETER(InitialSoilPConcentration)-PARAMETER(SoilInactivePConcentration)), PARAMETER(InitialEPC0));
				  
		double KfPar = PARAMETER(PhosphorousSorptionCoefficient);
				  
		if(PARAMETER(CalibrationMode)) return Kf;
		
		return KfPar; // If not in calibration mode, just set back the parameter provided by the user.
	)

	auto SoilWaterEPC0            = RegisterEquation(Model, "Soil water EPC0", MgPerL);
	SetInitialValue(Model, SoilWaterEPC0, InitialEPC0);

	auto InitialSoilTDPMass     = RegisterEquationInitialValue(Model, "Initial soil TDP mass", KgPerKm2);
	auto SoilTDPMass            = RegisterEquation(Model, "Soil TDP mass", KgPerKm2);
	SetInitialValue(Model, SoilTDPMass, InitialSoilTDPMass);
	
	auto InitialSoilLabilePMass = RegisterEquationInitialValue(Model, "Initial soil labile P mass", KgPerKm2);
	auto SoilLabilePMass = RegisterEquation(Model, "Soil labile P mass", KgPerKm2);
	SetInitialValue(Model, SoilLabilePMass, InitialSoilLabilePMass);
	
	EQUATION(Model, InitialSoilTDPMass,
		return RESULT(SoilWaterEPC0) * RESULT(SoilWaterVolume);   //NOTE: mg/l * mm = kg/km2
	)
	
	/*
	NOTE: The soil TDP mass is described by the ODE equation
	
		d(TDPs)/dt  = input - Kf*Msoil*(TDPs/Vs - EPC0) - Q*TDPs/Vs
	
	This equation is generally stiff. However, if we assume that Q (soil water flow, including quick flow) and V are approximately constant over the time step, we have an equation on the form
	
		d(TDPs)/dt  = (input + Kf*Msoil*EPC0)  -  ((Kf*Msoil + Q) / V)*TDPs
		            = a - b*TDPs
	This has the exact solution TDPs(t) = a/b + (TDPs(0) - a/b) * exp(-b*t)   , where we can insert t=1 to integrate over the time step.
	
	Solving it this way saves time by a factor of about 50-100, and has miniscule error compared to solving it with time-variable V and Q.
	
	Now, the soil labile P mass is described by
		
		d(Plab)/dt  = Kf*Msoil*((TDPs/Vs)-EPC0)
		
	So
		Plab(1) = Plab(0) + Integral(Kf*Msoil*((TDPs(t)/Vs) - EPC0), t=0->1)
	
	Again, assuming constant V, the integral will be
		I = (Kf*Msoil)*( (1/Vs)*Integral(TDPs(t), t=0->1) - EPC0)
		
		  = (Kf*Msoil)*( (1/Vs)(a/b + (TDPs(0)-a/b)*(1/b)*(1 - exp(-b)) ) - EPC0)
		  = (Kf*Msoil)*( (1/(Vs*b))(a + (TDPs(0) - a/b)(1 - exp(-b)) ) - EPC0)
	*/
	
	EQUATION(Model, SoilTDPMass,
		double Msoil = PARAMETER(MSoilPerM2) * 1e6;
		double Kf = PARAMETER(PhosphorousSorptionCoefficient);
		double Q  = LAST_RESULT(SoilWaterFlow) + RESULT(QuickFlow);
		double V  = LAST_RESULT(SoilWaterVolume);
		double epc0 = RESULT(SoilWaterEPC0);
		double input = IF_INPUT_ELSE_PARAMETER(NetAnnualPInputTimeseries, NetAnnualPInput) * 100.0 / 365.0;
		double TDP0 = LAST_RESULT(SoilTDPMass);
		
		double a = (input + Kf*Msoil*epc0);
		double b = (Kf*Msoil + Q) / V;
		double value = a/b + (TDP0 - a/b) * exp(-b);
		
		if(!PARAMETER(DynamicEPC0)) return PARAMETER(InitialEPC0)*V; //TDP0;
		
		return value;
	)
	
	EQUATION(Model, InitialSoilLabilePMass,
		double Msoil = PARAMETER(MSoilPerM2) * 1e6;
		return 1e-6 * (PARAMETER(InitialSoilPConcentration)-PARAMETER(SoilInactivePConcentration)) * Msoil;
	)
	
	EQUATION(Model, SoilLabilePMass,
		double Msoil = PARAMETER(MSoilPerM2) * 1e6;
		double Kf = PARAMETER(PhosphorousSorptionCoefficient);
		double Q  = LAST_RESULT(SoilWaterFlow) + RESULT(QuickFlow);
		double V  = LAST_RESULT(SoilWaterVolume);
		double epc0 = RESULT(SoilWaterEPC0);
		double input = IF_INPUT_ELSE_PARAMETER(NetAnnualPInputTimeseries, NetAnnualPInput) * 100.0 / 365.0;
		double TDP0 = LAST_RESULT(SoilTDPMass);
		
		//NOTE: we could factor out computation of a and b for reuse in the two equations, but they don't have any interesting meaning of their own.
		double a = (input + Kf*Msoil*epc0);
		double bV = (Kf*Msoil + Q);
		double b = bV / V;
	
		double sorp = (Kf*Msoil)*( (1/bV)*(a + (TDP0 - a/b)*(1 - exp(-b))) - epc0);

		if(!std::isfinite(sorp)) sorp = 0.0; //NOTE: computation can break down in some rare cases.
		
		if(!PARAMETER(DynamicEPC0)) sorp = 0.0;
	
		return LAST_RESULT(SoilLabilePMass) + sorp;
	)
	
	EQUATION(Model, SoilWaterEPC0,
		double Msoil = PARAMETER(MSoilPerM2) * 1e6;
		double Kf = PARAMETER(PhosphorousSorptionCoefficient);
		double Plabile = LAST_RESULT(SoilLabilePMass);
		
		if(PARAMETER(DynamicEPC0)) return SafeDivide(Plabile, (Kf * Msoil));
		
		return LAST_RESULT(SoilWaterEPC0);
	)
	
	// Post-processing soil P equations (convert units)
	auto SoilWaterTDPConcentration = RegisterEquation(Model, "Soil water TDP concentration", MgPerL);
	auto SoilLabilePConcentration  = RegisterEquation(Model, "Soil labile P concentration", MgPerKg);
	
	EQUATION(Model, SoilWaterTDPConcentration,
		double dynamicTDPConc = SafeDivide(RESULT(SoilTDPMass), RESULT(SoilWaterVolume));  // (kg/km2)/mm = mg/l
		double constantTDPConc = PARAMETER(InitialEPC0);
		
		if(!PARAMETER(DynamicEPC0)) return constantTDPConc;
		return dynamicTDPConc;		
	)
	
	EQUATION(Model, SoilLabilePConcentration,
		double labilePMassMg = 1e6 * RESULT(SoilLabilePMass);
		double Msoil = PARAMETER(MSoilPerM2) * 1e6;
		double constantLabilePConc = PARAMETER(InitialSoilPConcentration);
		
		if(!PARAMETER(DynamicEPC0)) return constantLabilePConc;
		return labilePMassMg/Msoil;
	)
	
	auto SoilTDPOutput = RegisterEquation(Model, "Soil TDP output", KgPerKm2PerDay);
	auto TotalSoilTDPOutput = RegisterEquationCumulative(Model, "Total soil TDP output", SoilTDPOutput, LandscapeUnits, LandUseProportions);
	
	EQUATION(Model, SoilTDPOutput,
		//TODO: Should consider using "integrated" values here similar to in the equation for the TDP mass value..
		double flow = (1.0-PARAMETER(BaseflowIndex)) * RESULT(SoilWaterFlow) + RESULT(QuickFlow);
		//return SafeDivide(RESULT(SoilTDPMass), RESULT(SoilWaterVolume)) * flow; 
		return RESULT(SoilWaterTDPConcentration) * flow;
	)
	
	auto ReachSolver = GetSolverHandle(Model, "SimplyQ reach solver");
	
	// Reach equations	
	
	auto ReachPPInputCoefficient = RegisterEquation(Model, "Reach PP input coefficent", KgPerDay);
	auto TotalReachPPInputCoefficient = RegisterEquationCumulative(Model, "Reach PP input coefficent summed over landscape units", ReachPPInputCoefficient, LandscapeUnits, LandUseProportions);
	
	auto StreamTDPFlux = RegisterEquation(Model, "Reach TDP flux", KgPerDay, ReachSolver);
	auto StreamPPFlux  = RegisterEquation(Model, "Reach PP flux", KgPerDay, ReachSolver);
	auto StreamTDPMass = RegisterEquationODE(Model, "Reach TDP mass", Kg, ReachSolver);
	SetInitialValue(Model, StreamTDPMass, 0.0);
	auto StreamPPMass  = RegisterEquationODE(Model, "Reach PP mass", Kg, ReachSolver);
	SetInitialValue(Model, StreamPPMass, 0.0);
	auto DailyMeanStreamTDPFlux = RegisterEquationODE(Model, "Reach daily mean TDP flux", KgPerDay, ReachSolver);
	ResetEveryTimestep(Model, DailyMeanStreamTDPFlux);
	auto DailyMeanStreamPPFlux = RegisterEquationODE(Model, "Reach daily mean PP flux", KgPerDay, ReachSolver);
	ResetEveryTimestep(Model, DailyMeanStreamPPFlux);
	
	auto ReachTDPInputFromUpstream  = RegisterEquation(Model, "Reach TDP input from upstream", KgPerDay);
	auto ReachTDPInputFromCatchment = RegisterEquation(Model, "Reach TDP input from catchment", KgPerDay, ReachSolver);
	auto ReachPPInputFromErosion    = RegisterEquation(Model, "Reach PP input from erosion and entrainment", KgPerDay, ReachSolver);
	auto ReachPPInputFromUpstream  = RegisterEquation(Model, "Reach PP input from upstream", KgPerDay);
	
	EQUATION(Model, StreamTDPFlux,
		return 86400.0 * RESULT(ReachFlow) * SafeDivide(RESULT(StreamTDPMass), RESULT(ReachVolume));
	)
	
	EQUATION(Model, DailyMeanStreamTDPFlux,
		return RESULT(StreamTDPFlux);
	)
	
	EQUATION(Model, StreamPPFlux,
		return 86400.0 * RESULT(ReachFlow) * SafeDivide(RESULT(StreamPPMass), RESULT(ReachVolume));
	)
	
	EQUATION(Model, DailyMeanStreamPPFlux,
		return RESULT(StreamPPFlux);
	)
	
	EQUATION(Model, ReachTDPInputFromUpstream,
		double upstreamflux = 0.0;
		for(index_t Input : BRANCH_INPUTS(Reach))
			upstreamflux += RESULT(DailyMeanStreamTDPFlux, Input);
		
		return upstreamflux;
	)
	
	EQUATION(Model, ReachTDPInputFromCatchment,
		return
			  RESULT(TotalSoilTDPOutput) * PARAMETER(CatchmentArea)
			+ RESULT(GroundwaterFlow) * ConvertMgPerLToKgPerMm(PARAMETER(GroundwaterTDPConcentration), PARAMETER(CatchmentArea))
			+ IF_INPUT_ELSE_PARAMETER(EffluentTDPTimeseries, EffluentTDP);
	)
	
	EQUATION(Model, StreamTDPMass,
		return
			  RESULT(ReachTDPInputFromCatchment)
			+ RESULT(ReachTDPInputFromUpstream)
			- RESULT(StreamTDPFlux);
	)
	
	EQUATION(Model, ReachPPInputCoefficient,
		double Msoil = PARAMETER(MSoilPerM2) * 1e6;
		double P_inactive = 1e-6*PARAMETER(SoilInactivePConcentration)*Msoil;
		return (RESULT(SoilLabilePMass) + P_inactive) * PARAMETER(CatchmentArea) * RESULT(ReachSedimentInputCoefficient);
	)
	
	EQUATION(Model, ReachPPInputFromErosion,
		double Msoil = PARAMETER(MSoilPerM2) * 1e6 * PARAMETER(CatchmentArea);
		
		return RESULT(ErosionFactor) * PARAMETER(PPEnrichmentFactor) * RESULT(TotalReachPPInputCoefficient) / Msoil;
	)
	
	EQUATION(Model, ReachPPInputFromUpstream,
		double upstreamflux = 0.0;
		for(index_t Input : BRANCH_INPUTS(Reach))
			upstreamflux += RESULT(DailyMeanStreamPPFlux, Input);
		
		return upstreamflux;
	)
	
	EQUATION(Model, StreamPPMass,
		return
			  RESULT(ReachPPInputFromErosion)
			+ RESULT(ReachPPInputFromUpstream)
			- RESULT(StreamPPFlux);
	)
	
	
	// Post-processing equations (mostly unit conversions)

	auto DailyMeanStreamTPFlux = RegisterEquation(Model, "Reach daily mean TP flux", KgPerDay);
	auto DailyMeanStreamSRPFlux = RegisterEquation(Model, "Reach daily mean SRP flux", KgPerDay);
	auto DailyMeanStreamTRPFlux = RegisterEquation(Model, "Reach daily mean TRP flux", KgPerDay);
	auto TDPConcentration = RegisterEquation(Model, "Reach TDP concentration", MgPerL); //Volume-weighted daily mean
	auto PPConcentration  = RegisterEquation(Model, "Reach PP concentration", MgPerL); //Volume-weighted daily mean
	auto TPConcentration  = RegisterEquation(Model, "Reach TP concentration", MgPerL); //Volume-weighted daily mean
	auto SRPConcentration = RegisterEquation(Model, "Reach SRP concentration", MgPerL); //Volume-weighted daily mean
	auto PRPConcentration = RegisterEquation(Model, "Reach PRP concentration", MgPerL); //Volume-weighted daily mean
	auto TRPConcentration = RegisterEquation(Model, "Reach TRP concentration", MgPerL); //Volume-weighted daily mean

	EQUATION(Model, DailyMeanStreamTPFlux,
		return RESULT(DailyMeanStreamTDPFlux) + RESULT(DailyMeanStreamPPFlux);
	)
	
	EQUATION(Model, DailyMeanStreamSRPFlux,
		return RESULT(DailyMeanStreamTDPFlux) * PARAMETER(SRPFraction);
	)
	
	EQUATION(Model, TDPConcentration,
		//Converting flow m3/s->m3/day, and converting kg/m3 to mg/l. TODO: make conversion functions
		return 1e3 * SafeDivide(RESULT(DailyMeanStreamTDPFlux), 86400.0 * RESULT(DailyMeanReachFlow));
	)
	
	EQUATION(Model, PPConcentration,
		//Converting flow m3/s->m3/day, and converting kg/m3 to mg/l. TODO: make conversion functions
		return 1e3 * SafeDivide(RESULT(DailyMeanStreamPPFlux), 86400.0 * RESULT(DailyMeanReachFlow));
	)
	
	EQUATION(Model, TPConcentration,
		return RESULT(TDPConcentration) + RESULT(PPConcentration);
	)
	
	// Calculate reactive P fractions
	// NOTE: the SRP and PRP fraction parameters will likely vary according to % land cover in a sub-catchment.
	// A future improvement could be to vary these parameters by land cover, and then use the area-weighted value here instead.
	
	EQUATION(Model, SRPConcentration,
		return RESULT(TDPConcentration) * PARAMETER(SRPFraction);
	)
	
	EQUATION(Model, PRPConcentration,
		return RESULT(PPConcentration) * PARAMETER(PRPFraction);
	)
	
	EQUATION(Model, TRPConcentration,
		return RESULT(SRPConcentration) + RESULT(PRPConcentration);
	)
	
	EQUATION(Model, DailyMeanStreamTRPFlux,
		return RESULT(DailyMeanStreamTDPFlux) * PARAMETER(SRPFraction) + RESULT(DailyMeanStreamPPFlux) * PARAMETER(PRPFraction);
	)
	
	EndModule(Model);
}

