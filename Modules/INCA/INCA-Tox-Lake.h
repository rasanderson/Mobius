
//NOTE: This is an extremely simple lake module that was made for just one specific project. It may not be widely applicable

inline double
AttnIndefiniteIntegral(double z, double a, double D)
{
	return exp(-a*z)*(a*(z-D)) / (a*a*D);
}

inline double
DiffusionCoefficient(double MolVol, double Temp)
{
	//Diffusion coefficient of contaminant in water. MolVol = molecular volume at boiling point, Temp = temperature Celsius.
	//Computation using Wilke Chang equation.
	
	double AbsT = 273.15 + Temp;
	return 4.09521908e-7 * AbsT * pow(MolVol, -0.6);
}


void
AddIncaToxLakeModule(mobius_model *Model)
{
	BeginModule(Model, "INCA-Tox Lake", "_dev");
	
	SetModuleDescription(Model, R""""(
This is a simple lake module for INCA-Tox that was made for just one specific project. It may not be widely applicable. Specifically, it does not work well with hydrophobic contaminants.
)"""");
	
	auto Dimensionless  = RegisterUnit(Model);
	auto Metres         = RegisterUnit(Model, "m");
	auto M2             = RegisterUnit(Model, "m2");
	auto Ng             = RegisterUnit(Model, "ng");
	auto NgPerM3        = RegisterUnit(Model, "ng/m3");
	auto NgPerKg        = RegisterUnit(Model, "ng/kg");
	auto NgPerDay       = RegisterUnit(Model, "ng/day");
	auto Days           = RegisterUnit(Model, "day");
	auto DegreesCelsius = RegisterUnit(Model, "°C");
	auto WPerM2         = RegisterUnit(Model, "W/m^2");
	auto HPa            = RegisterUnit(Model, "HPa");
	auto Percent        = RegisterUnit(Model, "%");
	auto KgPerM3        = RegisterUnit(Model, "kg/m3");
	auto Cm2PerS        = RegisterUnit(Model, "cm2/s");
	auto M3PerKg        = RegisterUnit(Model, "m3/kg");
	
	
	constexpr double ln2 = 0.69314718056;
	
	
	
	auto Reach        = GetIndexSetHandle(Model, "Reaches");
	auto Contaminant  = GetIndexSetHandle(Model, "Contaminants");
	
	auto LakeParams = RegisterParameterGroup(Model, "Additional lake params");
	
	
	auto SecchiDepth            = RegisterParameterDouble(Model, LakeParams, "Secchi depth", Metres, 2.0, 0.0, 10.0);
	auto DiffusiveExchangeScale = RegisterParameterDouble(Model, LakeParams, "Diffusive exchange scaling factor", Dimensionless, 1.0, 0.0, 10.0, "Scaling factor for diffusive exchange with atmosphere");
	auto SedimentThickness      = RegisterParameterDouble(Model, LakeParams, "Lake sediment thickness", Metres, 0.5, 0.0, 100.0, "Thickness of porous sediment");
	auto SedimentPorosity       = RegisterParameterDouble(Model, LakeParams, "Lake sediment porosity", Dimensionless, 0.92, 0.0, 1.0);
	auto SedSOCDensity          = RegisterParameterDouble(Model, LakeParams, "Lake sediment SOC density", KgPerM3, 300.0, 0.0, 1500.0, "Density in dry sediments");
	
	auto LakeTox   = RegisterParameterGroup(Model, "Lake contaminant", Contaminant);
	
	auto OpticalCrossection     = RegisterParameterDouble(Model, LakeTox, "Optical cross-section", Dimensionless, 255.0, 0.0, 1000.0, "For photo-degradation"); //TODO: Other unit
	
	//auto LakeSolver = GetSolverHandle(Model, "Lake solver");
	auto LakeContaminantSolver = RegisterSolver(Model, "Lake contaminant solver", 0.1, IncaDascru);
	
	auto EpilimnionDegradationTemperatureModifier  = RegisterEquation(Model, "Temperature modifier for degradation in epilimnion", Dimensionless, LakeContaminantSolver);
	auto HypolimnionDegradationTemperatureModifier = RegisterEquation(Model, "Temperature modifier for degradation in hypolimnion", Dimensionless, LakeContaminantSolver);
	
	auto DepositionOnLakeSurface                   = RegisterEquation(Model, "Deposition on lake surface", NgPerDay, LakeContaminantSolver);
	
	auto EpilimnionContaminantDegradation          = RegisterEquation(Model, "Epilimnion contaminant degradation", NgPerDay, LakeContaminantSolver);
	auto HypolimnionContaminantDegradation         = RegisterEquation(Model, "Hypolimnion contaminant degradation", NgPerDay, LakeContaminantSolver);
	
	auto EpilimnionContaminantMass                 = RegisterEquationODE(Model, "Epilimnion contaminant mass", Ng, LakeContaminantSolver);
	auto EpilimnionContaminantConc                 = RegisterEquation(Model, "Epilimnion contaminant concentration", NgPerM3, LakeContaminantSolver);
	
	auto HypolimnionContaminantMass                = RegisterEquationODE(Model, "Hypolimnion contaminant mass", Ng, LakeContaminantSolver);
	auto HypolimnionContaminantConc                = RegisterEquation(Model, "Hypolimnion contaminant concentration", NgPerM3, LakeContaminantSolver);
	
	auto LayerDiffusion                            = RegisterEquation(Model, "Lake layer contaminant diffusion", NgPerDay, LakeContaminantSolver);
	auto LayerExchange                             = RegisterEquation(Model, "Lake layer contaminant exchange", NgPerDay, LakeContaminantSolver);
	auto EpilimnionAttn                            = RegisterEquation(Model, "Epilimnion attenuation fraction", Dimensionless, LakeContaminantSolver);
	auto PhotoDegradation                          = RegisterEquation(Model, "Photo-degradation", NgPerDay, LakeContaminantSolver);
	
	auto DiffusiveAirLakeExchangeFlux              = RegisterEquation(Model, "Diffusive air-lake exchange flux", NgPerDay, LakeContaminantSolver);
	auto LakeContaminantFlux                       = RegisterEquation(Model, "Lake contaminant flux", NgPerDay, LakeContaminantSolver);
	
	auto OctanolWaterPartitioningCoefficientSed    = RegisterEquation(Model, "Octanol-water partitioning coefficient in lake sediments", Dimensionless, LakeContaminantSolver);
	auto WaterSOCPartitioningCoefficientSed        = RegisterEquation(Model, "Water-SOC partitioning coefficient in lake sediments", M3PerKg, LakeContaminantSolver);
	auto SedimentContaminantMass                   = RegisterEquationODE(Model, "Lake sediment contaminant mass", Ng, LakeContaminantSolver);
	auto LakeSedimentContaminantDiffusion          = RegisterEquation(Model, "Diffusive lake-sediment exchange flux", Ng, LakeContaminantSolver);
	auto SedimentContaminantDegradation            = RegisterEquation(Model, "Lake sediment contaminant degradation", NgPerDay, LakeContaminantSolver);
	auto SedimentPoreWaterContaminantConc          = RegisterEquation(Model, "Lake sediment pore water contaminant conc", NgPerM3, LakeContaminantSolver);
	auto SedimentSOCContaminantConc                = RegisterEquation(Model, "Lake sediment SOC contaminant conc", NgPerKg, LakeContaminantSolver);
	
	
	//PERSiST:
	auto ReachFlow                          = GetEquationHandle(Model, "Reach flow"); // m3/s
	
	//INCA-Tox
	
	//NOTE: This does not take account of any sediment-bound particles. Currently only appropriate for highly dissolvable contaminants.
	auto ReachContaminantFlux               = GetEquationHandle(Model, "Total reach contaminant flux");          // ng/day
	auto ContaminantInputFromLand           = GetEquationHandle(Model, "Total diffuse contaminant output");     // ng/day
	auto ContaminantInputFromUpstream       = GetEquationHandle(Model, "Reach contaminant input from upstream"); // ng/day
	//NOTE: We reuse these instead of computing them for the lake. This is ONLY OK because we have a slow-flowing river in our particular case!!
	auto AirWaterTransferVelocity           = GetEquationHandle(Model, "Reach overall air-water transfer velocity");
	auto AirWaterPartitioningCoefficient    = GetEquationHandle(Model, "Air-water partitioning coefficient (reach)");
	
	//EasyLake
	auto EpilimnionTemperature              = GetEquationHandle(Model, "Epilimnion temperature");
	auto HypolimnionTemperature             = GetEquationHandle(Model, "Mean hypolimnion temperature");
	auto LakeOutflow                        = GetEquationHandle(Model, "Lake outflow");
	auto EpilimnionVolume                   = GetEquationHandle(Model, "Epilimnion volume");
	auto HypolimnionVolume                  = GetEquationHandle(Model, "Hypolimnion volume");
	//auto MixingVelocity                     = GetEquationHandle(Model, "Mixing velocity");
	auto IsMixing                           = GetEquationHandle(Model, "The lake is being mixed");
	auto LakeSurfaceArea                    = GetEquationHandle(Model, "Lake surface area");
	auto LakeDepth                          = GetEquationHandle(Model, "Water level");
	auto EpilimnionThickness                = GetEquationHandle(Model, "Epilimnion thickness");
	auto EpilimnionShortwave                = GetEquationHandle(Model, "Epilimnion incoming shortwave radiation");
	auto IsIce                              = GetEquationHandle(Model, "There is ice");
	auto BottomTemperature                  = GetEquationHandle(Model, "Bottom temperature");
	auto MeanLakeTemperature                = GetEquationHandle(Model, "Mean lake temperature");
	
	auto AtmosphericContaminantConcentration           = GetParameterDoubleHandle(Model, "Atmospheric contaminant concentration");
	auto ReachContaminantHalfLife                      = GetParameterDoubleHandle(Model, "Reach contaminant half life");
	auto SedimentContaminantHalfLife                   = GetParameterDoubleHandle(Model, "Stream bed contaminant half life");  //TODO: this should be renamed in inca-tox to sediment.
	auto DegradationResponseToTemperature              = GetParameterDoubleHandle(Model, "Degradation rate response to 10°C change in temperature");
	auto TemperatureAtWhichDegradationRatesAreMeasured = GetParameterDoubleHandle(Model, "Temperature at which degradation rates are measured");
	auto MolecularVolume                               = GetParameterDoubleHandle(Model, "Contaminant molecular volume at surface pressure");
	auto Log10OctanolWaterPartitioningCoefficient25    = GetParameterDoubleHandle(Model, "Log10 Octanol-water partitioning coefficient at 25°C");
	auto OctanolWaterPhaseTransferEntalphy             = GetParameterDoubleHandle(Model, "Enthalpy of phase transfer between octanol and water");
	auto IsLake                                        = GetParameterBoolHandle(Model, "This section is a lake");
	
	auto AtmosphericContaminantConcentrationIn  = GetInputHandle(Model, "Atmospheric contaminant concentration");
	auto DepositionToLand                       = GetInputHandle(Model, "Contaminant deposition to land");
	auto DepositionToLandPar                    = GetParameterDoubleHandle(Model, "Deposition to land");


	auto ThisIsARiver                           = GetConditionalHandle(Model, "This is a river");
	auto ThisIsALake                            = GetConditionalHandle(Model, "This is a lake");
	
	auto SedimentSolver                         = GetSolverHandle(Model, "In-stream sediment solver");
	auto ReachContaminantSolver                 = GetSolverHandle(Model, "Reach contaminant solver");
	
	auto ReachShearVelocity                     = GetEquationHandle(Model, "Reach shear velocity");
	auto ProportionOfSedimentThatCanBeEntrained = GetEquationHandle(Model, "Proportion of sediment that can be entrained");
	auto StreamPower                            = GetEquationHandle(Model, "Stream power");
	auto ClayReleaseFromChannelBanks            = GetEquationHandle(Model, "Clay release from channel banks");
	auto TotalBedSedimentMassPerUnitArea        = GetEquationHandle(Model, "Total mass of bed sediment per unit area");
	auto PoreWaterVolume                        = GetEquationHandle(Model, "Pore water volume");
	auto DiffusivityOfDissolvedCompound         = GetEquationHandle(Model, "Diffusivity of dissolved compound in water");
	
	auto ReachSuspendedSOCFlux                  = GetEquationHandle(Model, "Reach suspended SOC flux");
	auto ReachSuspendedSOCMass                  = GetEquationHandle(Model, "Reach suspended SOC mass");
	auto BedSOCMass                             = GetEquationHandle(Model, "Stream bed SOC mass");
	auto ReachSOCDeposition                     = GetEquationHandle(Model, "Reach SOC deposition");
	auto ReachSOCEntrainment                    = GetEquationHandle(Model, "Reach SOC entrainment");
	auto TotalSOCFlux                           = GetEquationHandle(Model, "Total reach SOC flux");
	auto TotalSuspendedSOCMass                  = GetEquationHandle(Model, "Total suspended SOC mass");
	auto TotalBedSOCMass                        = GetEquationHandle(Model, "Total stream bed SOC mass");
	auto TotalSOCDeposition                     = GetEquationHandle(Model, "Total reach SOC deposition");
	auto TotalSOCEntrainment                    = GetEquationHandle(Model, "Total reach SOC entrainment");
	
	SetConditional(Model, SedimentSolver, ThisIsARiver);
	SetConditional(Model, ReachContaminantSolver, ThisIsARiver);
	SetConditional(Model, LakeContaminantSolver, ThisIsALake);
	
	SetConditional(Model, ReachShearVelocity, ThisIsARiver);
	SetConditional(Model, ProportionOfSedimentThatCanBeEntrained, ThisIsARiver);
	SetConditional(Model, StreamPower, ThisIsARiver);
	SetConditional(Model, ClayReleaseFromChannelBanks, ThisIsARiver);
	SetConditional(Model, ReachSuspendedSOCFlux, ThisIsARiver);
	SetConditional(Model, ReachSuspendedSOCMass, ThisIsARiver);
	SetConditional(Model, BedSOCMass, ThisIsARiver);
	SetConditional(Model, ReachSOCDeposition, ThisIsARiver);
	SetConditional(Model, ReachSOCEntrainment, ThisIsARiver);
	SetConditional(Model, TotalSOCFlux, ThisIsARiver);
	SetConditional(Model, TotalSuspendedSOCMass, ThisIsARiver);
	SetConditional(Model, TotalBedSOCMass, ThisIsARiver);
	SetConditional(Model, TotalSOCDeposition, ThisIsARiver);
	SetConditional(Model, TotalSOCEntrainment, ThisIsARiver);
	SetConditional(Model, TotalBedSedimentMassPerUnitArea, ThisIsARiver);
	SetConditional(Model, PoreWaterVolume, ThisIsARiver);
	SetConditional(Model, DiffusivityOfDissolvedCompound, ThisIsARiver);

	
	EQUATION_OVERRIDE(Model, ContaminantInputFromUpstream,
		double upstreamflux = 0.0;
		
		for(index_t Input : BRANCH_INPUTS(Reach))
		{
			if(PARAMETER(IsLake, Input))
				upstreamflux += RESULT(LakeContaminantFlux, Input);
			else
				upstreamflux += RESULT(ReachContaminantFlux, Input);
		}
		
		return upstreamflux;
	)
	
	
	
	EQUATION(Model, HypolimnionDegradationTemperatureModifier,
		return pow(PARAMETER(DegradationResponseToTemperature), 0.1*(RESULT(HypolimnionTemperature) - PARAMETER(TemperatureAtWhichDegradationRatesAreMeasured)));
	)
	
	EQUATION(Model, EpilimnionDegradationTemperatureModifier,
		return pow(PARAMETER(DegradationResponseToTemperature), 0.1*(RESULT(EpilimnionTemperature) - PARAMETER(TemperatureAtWhichDegradationRatesAreMeasured)));
	)
	
	EQUATION(Model, EpilimnionContaminantDegradation,
		return (ln2 / PARAMETER(ReachContaminantHalfLife)) * RESULT(EpilimnionContaminantMass) * RESULT(EpilimnionDegradationTemperatureModifier);
	)
	
	EQUATION(Model, HypolimnionContaminantDegradation,
		return (ln2 / PARAMETER(ReachContaminantHalfLife)) * RESULT(HypolimnionContaminantMass) * RESULT(HypolimnionDegradationTemperatureModifier);
	)
	
	EQUATION(Model, EpilimnionContaminantMass,
		
		return
			  RESULT(ContaminantInputFromLand)
			+ RESULT(ContaminantInputFromUpstream)
			+ RESULT(DepositionOnLakeSurface)
			- RESULT(LayerExchange)
			- RESULT(LayerDiffusion)
			- RESULT(LakeContaminantFlux)
			- RESULT(DiffusiveAirLakeExchangeFlux)
			- RESULT(PhotoDegradation)
			- RESULT(EpilimnionContaminantDegradation);
	)
	
	EQUATION(Model, DepositionOnLakeSurface,
		return IF_INPUT_ELSE_PARAMETER(DepositionToLand, DepositionToLandPar)*RESULT(LakeSurfaceArea);
	)
	
	EQUATION(Model, LakeContaminantFlux,
		return RESULT(EpilimnionContaminantConc) * RESULT(LakeOutflow) * 86400.0;
	)
	
	EQUATION(Model, EpilimnionContaminantConc,
		return SafeDivide(RESULT(EpilimnionContaminantMass), RESULT(EpilimnionVolume));
	)
	
	EQUATION(Model, HypolimnionContaminantMass,
		return RESULT(LayerExchange) + RESULT(LayerDiffusion) - RESULT(LakeSedimentContaminantDiffusion) - RESULT(HypolimnionContaminantDegradation);
	)
	
	EQUATION(Model, HypolimnionContaminantConc,
		return SafeDivide(RESULT(HypolimnionContaminantMass), RESULT(HypolimnionVolume));
	)
	
	EQUATION(Model, LayerDiffusion,
		double dz = 0.5*RESULT(LakeDepth);
		double diffusionCoeff = DiffusionCoefficient(RESULT(MeanLakeTemperature), PARAMETER(MolecularVolume)); //TODO; should use molecular volume at boiling point instead...
		return diffusionCoeff*86400.0*1e-4*RESULT(LakeSurfaceArea) * (RESULT(EpilimnionContaminantConc) - RESULT(HypolimnionContaminantConc)) * 0.5 / dz;
	)
	
	EQUATION(Model, LayerExchange,
		bool ismix = (bool)RESULT(IsMixing);
		double from_mixing = RESULT(LakeSurfaceArea)*RESULT(LakeDepth)*(RESULT(EpilimnionContaminantConc) - RESULT(HypolimnionContaminantConc));  //NOTE: Just have the speed large enough so that the mixing is "instant"
		double from_layer_thickening = -(RESULT(EpilimnionThickness)-LAST_RESULT(EpilimnionThickness))*RESULT(LakeSurfaceArea)*RESULT(HypolimnionContaminantConc);
		
		if(ismix) return from_mixing;
		return from_layer_thickening;
	)
	
	EQUATION(Model, PhotoDegradation,
		double oc_Nitro = PARAMETER(OpticalCrossection); // m2/mol Nitro            Optical cross-section of DOM
		double qy_Nitro = 0.45;  // mol Nitro /mol quanta   Quantum yield
		
		double f_uv = 0.06;      // Fract.              of PAR in incoming solar radiation
		double e_uv = 351843.0;  // J/mol               Average energy of Par photons
		
		//‒oc_Nitro * qy_Nitro f_par(1/e_par)*(86400)*Qsw*Attn_epilimnion * [Nitrosamines]" in mg N m-3 d-1
		double shortwave = RESULT(EpilimnionShortwave)*RESULT(EpilimnionAttn);
		return oc_Nitro * qy_Nitro * (f_uv / e_uv) * 86400.0 * shortwave * RESULT(EpilimnionContaminantMass);
	)
	
	
	EQUATION(Model, EpilimnionAttn,
		double a = 2.7/PARAMETER(SecchiDepth);
		double D = RESULT(LakeDepth);
		double d = RESULT(EpilimnionThickness);

		double top = AttnIndefiniteIntegral(0.0, a, D);
		double bot = AttnIndefiniteIntegral(d, a, D);
		
		double vv = (D - (1.0 - d/D)*(D-d));
		
		//return (bot - top);
		return 3.0*(bot - top)/vv;
	)
	
	EQUATION(Model, DiffusiveAirLakeExchangeFlux,
		double atmospheric = IF_INPUT_ELSE_PARAMETER(AtmosphericContaminantConcentrationIn, AtmosphericContaminantConcentration);
		double flux = RESULT(AirWaterTransferVelocity) * (RESULT(EpilimnionContaminantConc) - atmospheric/RESULT(AirWaterPartitioningCoefficient)) * RESULT(LakeSurfaceArea);
		if(RESULT(IsIce) > 0.0) flux = 0.0;
		return PARAMETER(DiffusiveExchangeScale)*flux;
	)
	
	
	EQUATION(Model, SedimentContaminantMass,
		return
			  RESULT(LakeSedimentContaminantDiffusion)
			- RESULT(SedimentContaminantDegradation);
	)
	
	EQUATION(Model, LakeSedimentContaminantDiffusion,
		double z_h_mean = (RESULT(LakeDepth) - RESULT(EpilimnionThickness))*0.5;
		double dz = z_h_mean + 0.5*PARAMETER(SedimentThickness);
		double diffusionCoeff = DiffusionCoefficient(RESULT(BottomTemperature), PARAMETER(MolecularVolume)); //TODO; should use molecular volume at boiling point instead...
		return diffusionCoeff*86400.0*1e-4*RESULT(LakeSurfaceArea) * (RESULT(HypolimnionContaminantConc) - RESULT(SedimentPoreWaterContaminantConc)) * 0.5 / dz;
	)
	
	EQUATION(Model, SedimentContaminantDegradation,
		double tempmod = pow(PARAMETER(DegradationResponseToTemperature), 0.1*(RESULT(BottomTemperature) - PARAMETER(TemperatureAtWhichDegradationRatesAreMeasured)));
		return (ln2 / PARAMETER(SedimentContaminantHalfLife)) * RESULT(SedimentContaminantMass) * tempmod;
	)
	
	EQUATION(Model, OctanolWaterPartitioningCoefficientSed,
		double LogKOW25 = PARAMETER(Log10OctanolWaterPartitioningCoefficient25);
		double R = 8.314; //Ideal gas constant (J K^-1 mol^-1)
		double tdiff = (1.0 / (RESULT(BottomTemperature)+273.15) - 1.0/(273.15 + 25.0));
		double LogKOWT = LogKOW25 - (1e3*PARAMETER(OctanolWaterPhaseTransferEntalphy) / (std::log(10.0)*R))   * tdiff;
		return std::pow(10.0, LogKOWT);
	)
	
	EQUATION(Model, WaterSOCPartitioningCoefficientSed,
		double densityofSOC = 1900.0; // kg/m^3
		double rOC = 0.41;   // Empirical constant.
		return RESULT(OctanolWaterPartitioningCoefficientSed) * rOC / densityofSOC;
	)
	
	EQUATION(Model, SedimentPoreWaterContaminantConc,
		double sed_volume = RESULT(LakeSurfaceArea) * PARAMETER(SedimentThickness); //NOTE: It is a bit awkward that this varies with the surface area...
		double phi = PARAMETER(SedimentPorosity);
		double porewater_volume = sed_volume * phi;
		double sed_soc_mass = sed_volume * (1.0 - phi) * PARAMETER(SedSOCDensity);
		return RESULT(SedimentContaminantMass) / (porewater_volume + RESULT(WaterSOCPartitioningCoefficientSed)*sed_soc_mass);
	)
	
	EQUATION(Model, SedimentSOCContaminantConc,
		return RESULT(SedimentPoreWaterContaminantConc) * RESULT(WaterSOCPartitioningCoefficientSed);
	)

	
	EndModule(Model);
}
