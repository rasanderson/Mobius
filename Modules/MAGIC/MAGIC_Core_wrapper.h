

/*
	This is a Mobius wrapper for the MAGIC core.
	
	This has to be used together with another "driver" module that determines things like temperature and external fluxes (nitrification, weathering, deposition etc. etc.)
*/



#include "MAGIC_Core.h"


static void
AddMagicCoreModel(mobius_model *Model)
{
	BeginModule(Model, "MAGIC Core", "0.1");
	
	SetModuleDescription(Model, R""""(
MAGIC is the Model of Acidification of Groundwater In Catchments.
	
The model has been published as
[B.J Cosby, R. C. Ferrier, A. Jenkins and R. F. Wright, 2001, Modelling the effects of acid deposition: refinements, adjustments and inclusion of nitrogen dynamics in the MAGIC model. Hydrol. Earth Syst. Sci, 5(3), 499-517](https://doi.org/10.5194/hess-5-499-2001)
	
This is a Mobius implementation. There are earlier implementations in FORTRAN by Bernard J. Cosby.
)"""");
	
	
	auto Dimensionless = RegisterUnit(Model);
	auto Ts            = RegisterUnit(Model, "months");
	auto M             = RegisterUnit(Model, "m");
	auto MPerTs        = RegisterUnit(Model, "m/month");
	auto Km2           = RegisterUnit(Model, "km2");
	auto KgPerM3       = RegisterUnit(Model, "kg/m3");
	auto MMolPerM2     = RegisterUnit(Model, "mmol/m2");
	auto MMolPerM3     = RegisterUnit(Model, "mmol/m3");
	auto MEqPerKg      = RegisterUnit(Model, "meq/kg");
	auto MEqPerM2      = RegisterUnit(Model, "meq/m2");
	auto MEqPerM2PerTs = RegisterUnit(Model, "meq/m2/month");
	auto MEqPerM3      = RegisterUnit(Model, "meq/m3");
	auto Log10MolPerL  = RegisterUnit(Model, "log10(mol/l)");
	auto MMolPerTs     = RegisterUnit(Model, "mmol/month");
	auto MMolPerM2PerTs = RegisterUnit(Model, "mmol/m2/month");
	auto DegreesCelsius	= RegisterUnit(Model, "°C");
	auto Percent        = RegisterUnit(Model, "%");
	
	auto ConvergenceParams         = RegisterParameterGroup(Model, "Convergence parameters");
	auto ConvergenceCriterion      = RegisterParameterDouble(Model, ConvergenceParams, "Convergence criterion", Dimensionless, 1.0, 0.01, 10.0, "Convergence criterion to stop solution routine, difference in total plus and total minus charges in solution NOTE: CONV = 1.0 is usual, but smaller values may be needed (at computational cost) if reliable pH's above 6-7 are needed");
	auto SolverStep                = RegisterParameterDouble(Model, ConvergenceParams, "Solver sub-step length", Ts, 0.1, 1e-3, 1.0, "Length of intermediate step between each time the solution is rebalanced");
	

	auto Compartment = RegisterIndexSet(Model, "Compartment");
	auto CompartmentParams = RegisterParameterGroup(Model, "Compartment parameters", Compartment);
	
	auto IsSoil                    = RegisterParameterBool(Model, CompartmentParams, "This is a soil compartment", true);
	auto RelativeArea              = RegisterParameterDouble(Model, CompartmentParams, "Relative area", Dimensionless, 1.0, 0.0, 1.0, "The fraction of the catchment covered by this compartment");
	auto Depth                     = RegisterParameterDouble(Model, CompartmentParams, "Depth", M, 1.0, 0.0, 100.0, "", "DEP");
	auto Porosity                  = RegisterParameterDouble(Model, CompartmentParams, "Porosity", Dimensionless, 0.5, 0.0, 1.0, "", "PV");
	auto BulkDensity               = RegisterParameterDouble(Model, CompartmentParams, "Bulk density", KgPerM3, 600.0, 0.0, 2000.0, "", "BD");
	auto CationExchangeCapacity    = RegisterParameterDouble(Model, CompartmentParams, "Cation exchange capacity", MEqPerKg, 100.0, 0.0, 500.0, "", "CEC");
	auto SO4HalfSat                = RegisterParameterDouble(Model, CompartmentParams, "Soil sulfate adsorption capacity, half saturation", MEqPerM3, 0.0, 0.0, 1000.0, "", "SO4half");
	auto SO4MaxCap                 = RegisterParameterDouble(Model, CompartmentParams, "Soil sulfate adsorption max capacity", MEqPerKg, 0.0, 0.0, 1.0, "", "SO4max");
	auto Log10AlOH3EquilibriumConst= RegisterParameterDouble(Model, CompartmentParams, "(log10) Al(OH)3 dissociation equilibrium constant", Dimensionless, 0.0, -10.0, 10.0, "", "KAl");
	auto HAlOH3Exponent            = RegisterParameterDouble(Model, CompartmentParams, "Al(OH)3 dissociation equation exponent", Dimensionless, 3.0, 1.0, 5.0, "", "SAl");
	auto PK1DOC                    = RegisterParameterDouble(Model, CompartmentParams, "(-log10) pK 1st equilibrium constant for triprotic organic acid", Dimensionless, 0.0, -10.0, 10.0);
	auto PK2DOC                    = RegisterParameterDouble(Model, CompartmentParams, "(-log10) pK 2nd equilibrium constant for triprotic organic acid", Dimensionless, 0.0, -10.0, 10.0);
	auto PK3DOC                    = RegisterParameterDouble(Model, CompartmentParams, "(-log10) pK 3rd equilibrium constant for triprotic organic acid", Dimensionless, 0.0, -10.0, 10.0);
	auto PK1AlDOC                  = RegisterParameterDouble(Model, CompartmentParams, "(-log10) pK Al(A) equilibrium constant for [(Al3+)(A3-)]", Dimensionless, 0.0, -10.0, 10.0);
	auto PK2AlDOC                  = RegisterParameterDouble(Model, CompartmentParams, "(-log10) pK Al(HA)+ equilibrium constant for [(Al3+)(HA2-)+]", Dimensionless, 0.0, -10.0, 10.0);
	
	
	auto InitialECa            = RegisterParameterDouble(Model, CompartmentParams, "Initial exchangeable Ca on soil as % of CEC", Percent, 0.0, 0.0, 100.0, "", "ECa");
	auto InitialEMg            = RegisterParameterDouble(Model, CompartmentParams, "Initial exchangeable Mg on soil as % of CEC", Percent, 0.0, 0.0, 100.0, "", "EMg");
	auto InitialENa            = RegisterParameterDouble(Model, CompartmentParams, "Initial exchangeable Na on soil as % of CEC", Percent, 0.0, 0.0, 100.0, "", "ENa");
	auto InitialEK             = RegisterParameterDouble(Model, CompartmentParams, "Initial exchangeable K on soil as % of CEC", Percent, 0.0, 0.0, 100.0, "", "EK");
	
	
	auto FlowFractions = RegisterParameterGroup(Model, "Flow fractions", Compartment, Compartment);
	
	auto FlowFraction = RegisterParameterDouble(Model, FlowFractions, "Flow fraction", Dimensionless, 0.0, 0.0, 1.0, "How large of a fraction of the discharge of this compartment (the row) goes to another compartment (the column)");
	
	
	auto CompartmentSolver = RegisterSolver(Model, "Compartment solver", SolverStep, MobiusEuler);
	
	auto ConcCa            = RegisterEquation(Model, "Ca(2+) ionic concentration", MEqPerM3);
	auto ConcMg            = RegisterEquation(Model, "Mg(2+) ionic concentration", MEqPerM3);
	auto ConcNa            = RegisterEquation(Model, "Na(+) ionic concentration", MEqPerM3);
	auto ConcK             = RegisterEquation(Model, "K(+) ionic concentration", MEqPerM3);
	auto ConcNH4           = RegisterEquation(Model, "NH4(+) ionic concentration", MEqPerM3);
	auto ConcSO4           = RegisterEquation(Model, "SO4(2-) ionic concentration", MEqPerM3);
	auto ConcCl            = RegisterEquation(Model, "Cl(-) ionic concentration", MEqPerM3);
	auto ConcNO3           = RegisterEquation(Model, "NO3(-) ionic concentration", MEqPerM3);
	auto ConcF             = RegisterEquation(Model, "F(-) ionic concentration", MEqPerM3);
	auto ConcPO4           = RegisterEquation(Model, "PO4(3-) ionic concentration", MEqPerM3);
	
	auto ConcAllSO4        = RegisterEquation(Model, "Total Sulfate in solution (ionic + Al complexes)", MEqPerM3);  //TODO!
	auto ConcAllF          = RegisterEquation(Model, "Total Fluoride in solution (ionic + Al complexes)", MEqPerM3);
	
	auto PH                = RegisterEquation(Model, "pH", Log10MolPerL);
	auto SumBaseCationConc = RegisterEquation(Model, "Sum of base cation concentrations (Ca + Mg + Na + K)", MEqPerM3);
	auto SumAcidAnionConc  = RegisterEquation(Model, "Sum of acid anion concentrations (SO4 + Cl + NO3 + F)", MEqPerM3);
	auto ChargeBalanceAlk  = RegisterEquation(Model, "Charge balance alkalinity", MEqPerM3);
	auto WeakAcidAlk       = RegisterEquation(Model, "Weak acid alkalinity", MEqPerM3);

	auto ExchangeableCa    = RegisterEquation(Model, "Exchangeable Ca on soil as % of CEC", Percent);
	auto ExchangeableMg    = RegisterEquation(Model, "Exchangeable Mg on soil as % of CEC", Percent);
	auto ExchangeableNa    = RegisterEquation(Model, "Exchangeable Na on soil as % of CEC", Percent);
	auto ExchangeableK     = RegisterEquation(Model, "Exchangeable K on soil as % of CEC", Percent);
	auto ExchangeableSO4   = RegisterEquation(Model, "Exchangeable SO4 on soil as % of max cap", Percent);
	auto BaseSaturationSoil = RegisterEquation(Model, "Base saturation of soil (ECa + EMg + ENa + EK)", Percent);

	auto ConcHCO3          = RegisterEquation(Model, "HCO3 (Bicarbonate) ionic concentration", MMolPerM3);
	auto ConcCO3           = RegisterEquation(Model, "CO3 (Carbonate) ionic concentration", MMolPerM3);
	auto ConcAl            = RegisterEquation(Model, "Al(3+) ionic concentration", MMolPerM3);
	auto ConcAllAl         = RegisterEquation(Model, "Total aluminum in solution (ionic + SO4-F-DOC complexes)", MMolPerM3);
	auto ConcOrgAl         = RegisterEquation(Model, "Aluminum in solution as organic complexes (AlA, Al(HA)(+)", MMolPerM3);

	auto ConcH2AM          = RegisterEquation(Model, "[H2A-] Monovalent ion concentration, triprotic organic acid", MMolPerM3);
	auto ConcHA2M          = RegisterEquation(Model, "[HA2-] Divalent ion concentration, triprotic organic acid", MMolPerM3);
	auto ConcA3M           = RegisterEquation(Model, "[A3-] Trivalent ion concentration, triprotic organic acid", MMolPerM3);
	
	auto SumPositive       = RegisterEquation(Model, "Sum of all positive charges in solution (cations)", MEqPerM3);
	auto SumNegative       = RegisterEquation(Model, "Sum of all negative charges in solution (anions)", MEqPerM3);
	
	auto ConcAllDIC        = RegisterEquation(Model, "Total anionic charge from inorganic carbon in solution (HC3O, CO3)", MEqPerM3);
	auto ConcAllDOC        = RegisterEquation(Model, "Total anionic charge from DOC (triprotic acid) in solution (H2AM, HA2M, A3M)", MEqPerM3);
	auto CaAlRatio         = RegisterEquation(Model, "Ca ion to aqueous Al molar ratio", Dimensionless);
	
	auto ConcH         = RegisterEquation(Model, "H(+) ionic concentration", MMolPerM3, CompartmentSolver);
	SetInitialValue(Model, ConcH, 0.0); //NOTE: This is overwritten by InitialSO4, but we have to set this because the initial value system doesn't understand complexities related to EquationIsComputedBy (sigh!)
	auto IonicStrength = RegisterEquation(Model, "Ionic strength", Dimensionless);
	//SetInitialValue(Model, IonicStrength, 0.0); //NOTE: This is computed in the initial value step instead.
	
	
	auto CaInput       = RegisterEquation(Model, "Ca input from other compartments", MEqPerM2PerTs);
	auto MgInput       = RegisterEquation(Model, "Mg input from other compartments", MEqPerM2PerTs);
	auto NaInput       = RegisterEquation(Model, "Na input from other compartments", MEqPerM2PerTs);
	auto KInput        = RegisterEquation(Model, "K input from other compartments", MEqPerM2PerTs);
	auto NH4Input      = RegisterEquation(Model, "NH4 input from other compartments", MEqPerM2PerTs);
	auto SO4Input      = RegisterEquation(Model, "SO4 input from other compartments", MEqPerM2PerTs);
	auto ClInput       = RegisterEquation(Model, "Cl input from other compartments", MEqPerM2PerTs);
	auto NO3Input      = RegisterEquation(Model, "NO3 input from other compartments", MEqPerM2PerTs);
	auto FInput        = RegisterEquation(Model, "F input from other compartments", MEqPerM2PerTs);
	auto PO4Input      = RegisterEquation(Model, "PO4 input from other compartments", MEqPerM2PerTs);
	
	
	auto CaOutput      = RegisterEquation(Model, "Ca output via discharge", MEqPerM2PerTs, CompartmentSolver);
	auto MgOutput      = RegisterEquation(Model, "Mg output via discharge", MEqPerM2PerTs, CompartmentSolver);
	auto NaOutput      = RegisterEquation(Model, "Na output via discharge", MEqPerM2PerTs, CompartmentSolver);
	auto KOutput       = RegisterEquation(Model, "K output via discharge", MEqPerM2PerTs, CompartmentSolver);
	auto NH4Output     = RegisterEquation(Model, "NH4 output via discharge", MEqPerM2PerTs, CompartmentSolver);
	auto SO4Output     = RegisterEquation(Model, "SO4 output via discharge", MEqPerM2PerTs, CompartmentSolver);
	auto ClOutput      = RegisterEquation(Model, "Cl output via discharge", MEqPerM2PerTs, CompartmentSolver);
	auto NO3Output     = RegisterEquation(Model, "NO3 output via discharge", MEqPerM2PerTs, CompartmentSolver);
	auto FOutput       = RegisterEquation(Model, "F output via discharge", MEqPerM2PerTs, CompartmentSolver);
	auto PO4Output     = RegisterEquation(Model, "PO4 output via discharge", MEqPerM2PerTs, CompartmentSolver);
	
	auto TotalCa       = RegisterEquationODE(Model, "Total Ca mass", MEqPerM2, CompartmentSolver);
	auto TotalMg       = RegisterEquationODE(Model, "Total Mg mass", MEqPerM2, CompartmentSolver);
	auto TotalNa       = RegisterEquationODE(Model, "Total Na mass", MEqPerM2, CompartmentSolver);
	auto TotalK        = RegisterEquationODE(Model, "Total K  mass", MEqPerM2, CompartmentSolver);
	auto TotalNH4      = RegisterEquationODE(Model, "Total NH4 mass", MEqPerM2, CompartmentSolver);
	auto TotalSO4      = RegisterEquationODE(Model, "Total SO4 mass", MEqPerM2, CompartmentSolver);
	auto TotalCl       = RegisterEquationODE(Model, "Total Cl mass", MEqPerM2, CompartmentSolver);
	auto TotalNO3      = RegisterEquationODE(Model, "Total NO3 mass", MEqPerM2, CompartmentSolver);
	auto TotalF        = RegisterEquationODE(Model, "Total F mass", MEqPerM2, CompartmentSolver);
	auto TotalPO4      = RegisterEquationODE(Model, "Total PO4 mass", MEqPerM2, CompartmentSolver);
	
	
	// Equations that have to be defined by an outside "driver", and are not provided in the core, but which the core has to read the values of:
	auto Discharge          = RegisterEquation(Model, "Discharge", MPerTs);
	auto Temperature        = RegisterEquation(Model, "Temperature", DegreesCelsius);
	auto PartialPressureCO2 = RegisterEquation(Model, "CO2 partial pressure", Percent);
	auto OAConcentration    = RegisterEquation(Model, "Organic acid concentration", MMolPerM3);
	
	auto CaExternalFlux     = RegisterEquation(Model, "Sum of Ca fluxes not related to discharge", MEqPerM2PerTs, CompartmentSolver);
	auto MgExternalFlux     = RegisterEquation(Model, "Sum of Mg fluxes not related to discharge", MEqPerM2PerTs);
	auto NaExternalFlux     = RegisterEquation(Model, "Sum of Na fluxes not related to discharge", MEqPerM2PerTs);
	auto KExternalFlux      = RegisterEquation(Model, "Sum of K fluxes not related to discharge", MEqPerM2PerTs);
	auto NH4ExternalFlux    = RegisterEquation(Model, "Sum of NH4 fluxes not related to discharge", MEqPerM2PerTs);
	auto SO4ExternalFlux    = RegisterEquation(Model, "Sum of SO4 fluxes not related to discharge", MEqPerM2PerTs);
	auto ClExternalFlux     = RegisterEquation(Model, "Sum of Cl fluxes not related to discharge", MEqPerM2PerTs);
	auto NO3ExternalFlux    = RegisterEquation(Model, "Sum of NO3 fluxes not related to discharge", MEqPerM2PerTs);
	auto FExternalFlux      = RegisterEquation(Model, "Sum of F fluxes not related to discharge", MEqPerM2PerTs);
	auto PO4ExternalFlux    = RegisterEquation(Model, "Sum of PO4 fluxes not related to discharge", MEqPerM2PerTs);
	
	auto InitialConcCa       = RegisterEquationInitialValue(Model, "Initial Ca concentration", MEqPerM3);
	SetInitialValue(Model, ConcCa, InitialConcCa);
	auto InitialConcMg       = RegisterEquationInitialValue(Model, "Initial Mg concentration", MEqPerM3);
	SetInitialValue(Model, ConcMg, InitialConcMg);
	auto InitialConcNa       = RegisterEquationInitialValue(Model, "Initial Na concentration", MEqPerM3);
	SetInitialValue(Model, ConcNa, InitialConcNa);
	auto InitialConcK        = RegisterEquationInitialValue(Model, "Initial K concentration", MEqPerM3);
	SetInitialValue(Model, ConcK, InitialConcK);
	auto InitialConcNH4      = RegisterEquationInitialValue(Model, "Initial NH4 concentration", MEqPerM3);
	SetInitialValue(Model, ConcNH4, InitialConcNH4);
	auto InitialConcAllSO4   = RegisterEquationInitialValue(Model, "Initial total sulfate in solution", MEqPerM3);
	SetInitialValue(Model, ConcAllSO4, InitialConcAllSO4);
	auto InitialConcCl       = RegisterEquationInitialValue(Model, "Initial Cl concentration", MEqPerM3);
	SetInitialValue(Model, ConcCl, InitialConcCl);
	auto InitialConcNO3      = RegisterEquationInitialValue(Model, "Initial NO3 concentration", MEqPerM3);
	SetInitialValue(Model, ConcNO3, InitialConcNO3);
	auto InitialConcAllF     = RegisterEquationInitialValue(Model, "Initial total fluoride in solution", MEqPerM3);
	SetInitialValue(Model, ConcAllF, InitialConcAllF);
	auto InitialConcPO4      = RegisterEquationInitialValue(Model, "Initial PO4 concentration", MEqPerM3);
	SetInitialValue(Model, ConcPO4, InitialConcPO4);
	
	auto InitialCa           = RegisterEquationInitialValue(Model, "Initial Ca mass", MEqPerM2);
	SetInitialValue(Model, TotalCa, InitialCa);
	auto InitialMg           = RegisterEquationInitialValue(Model, "Initial Mg mass", MEqPerM2);
	SetInitialValue(Model, TotalMg, InitialMg);
	auto InitialNa           = RegisterEquationInitialValue(Model, "Initial Na mass", MEqPerM2);
	SetInitialValue(Model, TotalNa, InitialNa);
	auto InitialK            = RegisterEquationInitialValue(Model, "Initial K mass", MEqPerM2);
	SetInitialValue(Model, TotalK, InitialK);
	auto InitialNH4          = RegisterEquationInitialValue(Model, "Initial NH4 mass", MEqPerM2);
	SetInitialValue(Model, TotalNH4, InitialNH4);
	auto InitialSO4          = RegisterEquationInitialValue(Model, "Initial SO4 mass", MEqPerM2);
	SetInitialValue(Model, TotalSO4, InitialSO4);
	auto InitialCl           = RegisterEquationInitialValue(Model, "Initial Cl mass", MEqPerM2);
	SetInitialValue(Model, TotalCl, InitialCl);
	auto InitialNO3          = RegisterEquationInitialValue(Model, "Initial NO3 mass", MEqPerM2);
	SetInitialValue(Model, TotalNO3, InitialNO3);
	auto InitialF            = RegisterEquationInitialValue(Model, "Initial F mass", MEqPerM2);
	SetInitialValue(Model, TotalF, InitialF);
	auto InitialPO4          = RegisterEquationInitialValue(Model, "Initial PO4 mass", MEqPerM2);
	SetInitialValue(Model, TotalPO4, InitialPO4);
	
	//NOTE: We need to force this equation to have itself as its initial value computation, otherwise it doesn't get evaluated and just has a 0 initial value, causing initial input fluxes to other compartments to be wrong.
	SetInitialValue(Model, CaOutput, CaOutput);
	SetInitialValue(Model, MgOutput, MgOutput);
	SetInitialValue(Model, NaOutput, NaOutput);
	SetInitialValue(Model, KOutput, KOutput);
	SetInitialValue(Model, NH4Output, NH4Output);
	SetInitialValue(Model, SO4Output, SO4Output);
	SetInitialValue(Model, ClOutput, ClOutput);
	SetInitialValue(Model, NO3Output, NO3Output);
	SetInitialValue(Model, FOutput, FOutput);
	SetInitialValue(Model, PO4Output, PO4Output);
	
	auto Log10CaAlSelectCoeff      = RegisterEquation(Model, "(log10) Ca/Al exchange selectivity coefficient", Dimensionless);
	auto Log10MgAlSelectCoeff      = RegisterEquation(Model, "(log10) Mg/Al exchange selectivity coefficient", Dimensionless);
	auto Log10NaAlSelectCoeff      = RegisterEquation(Model, "(log10) Na/Al exchange selectivity coefficient", Dimensionless);
	auto Log10KAlSelectCoeff       = RegisterEquation(Model, "(log10) K/Al exchange selectivity coefficient", Dimensionless);
	
	auto CationExchangeCapacityEq  = RegisterEquation(Model, "Cation exchange capacity", MEqPerKg);
	SetInitialValue(Model, CationExchangeCapacityEq, CationExchangeCapacity);
	
	constexpr double MinInitConc = 1e-6;   //NOTE: To safeguard against numerical errors.

	EQUATION(Model, InitialConcCa,
		//double val = (RESULT(CaExternalFlux) + RESULT(CaInput)) / (RESULT(Discharge) / PARAMETER(RelativeArea)); // Assume initial steady state
		//WarningPrint("Initial conc Ca ", val);
		//WarningPrint("Initial discharge ", RESULT(Discharge));
		//return val;
		//WarningPrint("Ca input ", RESULT(CaInput), "\n");
		//std::cout <<"external: "<< RESULT(CaExternalFlux)<<"\n";
		
		double Value = (RESULT(CaExternalFlux) + RESULT(CaInput)) / (RESULT(Discharge) / PARAMETER(RelativeArea)); // Assume initial steady state
		return std::max(Value, MinInitConc);
	)
	
	EQUATION(Model, InitialConcMg,
		double Value = (RESULT(MgExternalFlux) + RESULT(MgInput)) / (RESULT(Discharge) / PARAMETER(RelativeArea)); // Assume initial steady state
		return std::max(Value, MinInitConc);
	)
	
	EQUATION(Model, InitialConcNa,
		double Value = (RESULT(NaExternalFlux) + RESULT(NaInput)) / (RESULT(Discharge) / PARAMETER(RelativeArea)); // Assume initial steady state
		return std::max(Value, MinInitConc);
	)
	
	EQUATION(Model, InitialConcK,
		double Value = (RESULT(KExternalFlux) + RESULT(KInput)) / (RESULT(Discharge) / PARAMETER(RelativeArea)); // Assume initial steady state
		return std::max(Value, MinInitConc);
	)
	
	EQUATION(Model, InitialConcNH4,
		double Value = (RESULT(NH4ExternalFlux) + RESULT(NH4Input)) / (RESULT(Discharge) / PARAMETER(RelativeArea)); // Assume initial steady state
		return std::max(Value, MinInitConc);
	)
	
	EQUATION(Model, InitialConcAllSO4,
		double Value = (RESULT(SO4ExternalFlux) + RESULT(SO4Input)) / (RESULT(Discharge) / PARAMETER(RelativeArea)); // Assume initial steady state
		return std::max(Value, MinInitConc);
	)
	
	EQUATION(Model, InitialConcCl,
		double Value = (RESULT(ClExternalFlux) + RESULT(ClInput)) / (RESULT(Discharge) / PARAMETER(RelativeArea)); // Assume initial steady state
		return std::max(Value, MinInitConc);
	)
	
	EQUATION(Model, InitialConcNO3,
		double Value = (RESULT(NO3ExternalFlux) + RESULT(NO3Input)) / (RESULT(Discharge) / PARAMETER(RelativeArea)); // Assume initial steady state
		return std::max(Value, MinInitConc);
	)
	
	EQUATION(Model, InitialConcAllF,
		double Value = (RESULT(FExternalFlux) + RESULT(FInput)) / (RESULT(Discharge) / PARAMETER(RelativeArea)); // Assume initial steady state
		return std::max(Value, 1e-10);
	)
	
	EQUATION(Model, InitialConcPO4,
		double Value = (RESULT(PO4ExternalFlux) + RESULT(PO4Input)) / (RESULT(Discharge) / PARAMETER(RelativeArea)); // Assume initial steady state
		return std::max(Value, 1e-10);
	)
	
	EQUATION(Model, InitialCa,
		double initconc        = RESULT(ConcCa);
		double porosity        = PARAMETER(Porosity);
		double WaterVolume     = PARAMETER(Depth);
		if(PARAMETER(IsSoil)) WaterVolume *= porosity;
		double SCECECa         = PARAMETER(InitialECa)*0.01*PARAMETER(CationExchangeCapacity)*PARAMETER(Depth)*PARAMETER(BulkDensity);
		if(!PARAMETER(IsSoil)) SCECECa = 0.0;
		return SCECECa + WaterVolume*initconc;
	)
	
	EQUATION(Model, InitialMg,
		double initconc        = RESULT(ConcMg);
		double porosity        = PARAMETER(Porosity);
		double WaterVolume     = PARAMETER(Depth);
		if(PARAMETER(IsSoil)) WaterVolume *= porosity;
		double SCECEMg         = PARAMETER(InitialEMg)*0.01*PARAMETER(CationExchangeCapacity)*PARAMETER(Depth)*PARAMETER(BulkDensity);
		if(!PARAMETER(IsSoil)) SCECEMg = 0.0;
		return SCECEMg + WaterVolume*initconc;
	)
	
	EQUATION(Model, InitialNa,
		double initconc        = RESULT(ConcNa);
		double porosity        = PARAMETER(Porosity);
		double WaterVolume     = PARAMETER(Depth);
		if(PARAMETER(IsSoil)) WaterVolume *= porosity;
		double SCECENa         = PARAMETER(InitialENa)*0.01*PARAMETER(CationExchangeCapacity)*PARAMETER(Depth)*PARAMETER(BulkDensity);
		if(!PARAMETER(IsSoil)) SCECENa = 0.0;
		return SCECENa + WaterVolume*initconc;
	)
	
	EQUATION(Model, InitialK,
		double initconc        = RESULT(ConcK);
		double porosity        = PARAMETER(Porosity);
		double WaterVolume     = PARAMETER(Depth);
		if(PARAMETER(IsSoil)) WaterVolume *= porosity;
		double SCECEK          = PARAMETER(InitialEK)*0.01*PARAMETER(CationExchangeCapacity)*PARAMETER(Depth)*PARAMETER(BulkDensity);
		if(!PARAMETER(IsSoil)) SCECEK = 0.0;
		return SCECEK + WaterVolume*initconc;
	)
	
	EQUATION(Model, InitialNH4,
		double initconc        = RESULT(ConcNH4);
		double porosity        = PARAMETER(Porosity);
		double WaterVolume     = PARAMETER(Depth);
		if(PARAMETER(IsSoil)) WaterVolume *= porosity;
		return initconc * WaterVolume;
	)
	
	EQUATION(Model, InitialSO4,

		magic_param Param = {};
		magic_init_input Input = {};
		magic_init_result Result = {};
		
		Param.Depth              = PARAMETER(Depth);
		Param.Temperature        = RESULT(Temperature);
		Param.PartialPressureCO2 = RESULT(PartialPressureCO2);
		Param.conc_DOC           = RESULT(OAConcentration);
		
		Param.Log10AlOH3EquilibriumConst = PARAMETER(Log10AlOH3EquilibriumConst);
		Param.HAlOH3Exponent             = PARAMETER(HAlOH3Exponent);
		Param.pK1DOC                     = PARAMETER(PK1DOC);
		Param.pK2DOC                     = PARAMETER(PK2DOC);
		Param.pK3DOC                     = PARAMETER(PK3DOC);
		Param.pK1AlDOC                   = PARAMETER(PK1AlDOC);
		Param.pK2AlDOC                   = PARAMETER(PK2AlDOC);
		
		Param.Porosity                   = PARAMETER(Porosity);
		Param.BulkDensity                = PARAMETER(BulkDensity);
		Param.CationExchangeCapacity     = PARAMETER(CationExchangeCapacity);
		Param.SO4HalfSat                 = PARAMETER(SO4HalfSat);
		Param.SO4MaxCap                  = PARAMETER(SO4MaxCap);
		
		Input.conc_Ca  = RESULT(ConcCa) / 2.0;
		Input.conc_Mg  = RESULT(ConcMg) / 2.0;
		Input.conc_Na  = RESULT(ConcNa);
		Input.conc_K   = RESULT(ConcK);
		Input.conc_NH4 = RESULT(ConcNH4);
		Input.all_SO4  = RESULT(ConcAllSO4) / 2.0;
		Input.conc_Cl  = RESULT(ConcCl);
		Input.conc_NO3 = RESULT(ConcNO3);
		Input.all_F    = RESULT(ConcAllF);
		Input.conc_PO4 = RESULT(ConcPO4) / 3.0;   //TODO: Check if 3.0 is correct
		
		Input.exchangeable_Ca = PARAMETER(InitialECa)*0.01;
		Input.exchangeable_Mg = PARAMETER(InitialEMg)*0.01;
		Input.exchangeable_Na = PARAMETER(InitialENa)*0.01;
		Input.exchangeable_K  = PARAMETER(InitialEK)*0.01;
		
		bool issoil = PARAMETER(IsSoil);
		double conv = PARAMETER(ConvergenceCriterion);
		if(RunState__->Running)   // Safeguard so that we don't crash on initialisation run.
		{
			MagicCoreInitial(Input, Param, Result, issoil, conv);
		}

		SET_RESULT(Log10CaAlSelectCoeff, Result.Log10CaAlSelectCoeff);
		SET_RESULT(Log10MgAlSelectCoeff, Result.Log10MgAlSelectCoeff);
		SET_RESULT(Log10NaAlSelectCoeff, Result.Log10NaAlSelectCoeff);
		SET_RESULT(Log10KAlSelectCoeff, Result.Log10KAlSelectCoeff);
		SET_RESULT(IonicStrength, Result.IonicStrength);
		SET_RESULT(ConcH, Result.conc_H);
		
		double initconc = RESULT(ConcAllSO4); //Initial total concentration in solution.
		
		double porosity        = PARAMETER(Porosity);
		double WaterVolume     = PARAMETER(Depth);
		if(PARAMETER(IsSoil)) WaterVolume *= porosity;
		double SMESO4        = Result.exchangeable_SO4*PARAMETER(Depth)*PARAMETER(BulkDensity);
		if(!PARAMETER(IsSoil)) SMESO4 = 0.0;
		double total_SO4 = SMESO4 + WaterVolume*initconc;
		
		return total_SO4;
	)
	
	EQUATION(Model, InitialCl,
		double initconc        = RESULT(ConcCl);
		double porosity        = PARAMETER(Porosity);
		double WaterVolume     = PARAMETER(Depth);
		if(PARAMETER(IsSoil)) WaterVolume *= porosity;
		return initconc * WaterVolume;
	)

	EQUATION(Model, InitialNO3,
		double initconc        = RESULT(ConcNO3);
		double porosity        = PARAMETER(Porosity);
		double WaterVolume     = PARAMETER(Depth);
		if(PARAMETER(IsSoil)) WaterVolume *= porosity;
		return initconc * WaterVolume;
	)
	
	
	EQUATION(Model, InitialF,
		double initconc        = RESULT(ConcAllF);
		double porosity        = PARAMETER(Porosity);
		double WaterVolume     = PARAMETER(Depth);
		if(PARAMETER(IsSoil)) WaterVolume *= porosity;
		return initconc * WaterVolume;
	)
	
	EQUATION(Model, InitialPO4,
		double initconc        = RESULT(ConcPO4);
		double porosity        = PARAMETER(Porosity);
		double WaterVolume     = PARAMETER(Depth);
		if(PARAMETER(IsSoil)) WaterVolume *= porosity;
		return initconc * WaterVolume;
	)
	
	
	//NOTE: These stay constant through the run
	EQUATION(Model, Log10CaAlSelectCoeff,
		CURRENT_INDEX(Compartment);                  //NOTE: Otherwise, this doesn't have an index set dependency properly registered.
		return LAST_RESULT(Log10CaAlSelectCoeff);
	)
	
	EQUATION(Model, Log10MgAlSelectCoeff,
		CURRENT_INDEX(Compartment);
		return LAST_RESULT(Log10MgAlSelectCoeff);
	)
	
	EQUATION(Model, Log10NaAlSelectCoeff,
		CURRENT_INDEX(Compartment);
		return LAST_RESULT(Log10NaAlSelectCoeff);
	)
	
	EQUATION(Model, Log10KAlSelectCoeff,
		CURRENT_INDEX(Compartment);
		return LAST_RESULT(Log10KAlSelectCoeff);
	)
	
	
	EQUATION(Model, CaOutput,
		return RESULT(Discharge)*RESULT(ConcCa) / PARAMETER(RelativeArea);
	)
	
	EQUATION(Model, MgOutput,
		return RESULT(Discharge)*RESULT(ConcMg) / PARAMETER(RelativeArea);
	)
	
	EQUATION(Model, NaOutput,
		return RESULT(Discharge)*RESULT(ConcNa) / PARAMETER(RelativeArea);
	)
	
	EQUATION(Model, KOutput,
		return RESULT(Discharge)*RESULT(ConcK) / PARAMETER(RelativeArea);
	)
	
	EQUATION(Model, NH4Output,
		return RESULT(Discharge)*RESULT(ConcNH4) / PARAMETER(RelativeArea);
	)
	
	EQUATION(Model, SO4Output,
		return RESULT(Discharge)*RESULT(ConcAllSO4) / PARAMETER(RelativeArea);
	)
	
	EQUATION(Model, ClOutput,
		return RESULT(Discharge)*RESULT(ConcCl) / PARAMETER(RelativeArea);
	)
	
	EQUATION(Model, NO3Output,
		return RESULT(Discharge)*RESULT(ConcNO3) / PARAMETER(RelativeArea);
	)
	
	EQUATION(Model, FOutput,
		return RESULT(Discharge)*RESULT(ConcAllF) / PARAMETER(RelativeArea);
	)
	
	EQUATION(Model, PO4Output,
		return RESULT(Discharge)*RESULT(ConcPO4) / PARAMETER(RelativeArea);
	)
	
	EQUATION(Model, CaInput,
		double input = 0.0;
		index_t ThisCompartment = CURRENT_INDEX(Compartment);
		for(index_t OtherCompartment = FIRST_INDEX(Compartment); OtherCompartment < ThisCompartment; ++OtherCompartment)
		{
			input += RESULT(CaOutput, OtherCompartment)
			* PARAMETER(FlowFraction, OtherCompartment, ThisCompartment) * PARAMETER(RelativeArea, OtherCompartment);
		}
		return input / PARAMETER(RelativeArea);
	)
	
	EQUATION(Model, MgInput,
		double input = 0.0;
		index_t ThisCompartment = CURRENT_INDEX(Compartment);
		for(index_t OtherCompartment = FIRST_INDEX(Compartment); OtherCompartment < ThisCompartment; ++OtherCompartment)
		{
			input += RESULT(MgOutput, OtherCompartment) * PARAMETER(FlowFraction, OtherCompartment, ThisCompartment) * PARAMETER(RelativeArea, OtherCompartment);
		}
		return input / PARAMETER(RelativeArea);
	)
	
	EQUATION(Model, NaInput,
		double input = 0.0;
		index_t ThisCompartment = CURRENT_INDEX(Compartment);
		for(index_t OtherCompartment = FIRST_INDEX(Compartment); OtherCompartment < ThisCompartment; ++OtherCompartment)
		{
			input += RESULT(NaOutput, OtherCompartment) * PARAMETER(FlowFraction, OtherCompartment, ThisCompartment) * PARAMETER(RelativeArea, OtherCompartment);
		}
		return input / PARAMETER(RelativeArea);
	)
	
	EQUATION(Model, KInput,
		double input = 0.0;
		index_t ThisCompartment = CURRENT_INDEX(Compartment);
		for(index_t OtherCompartment = FIRST_INDEX(Compartment); OtherCompartment < ThisCompartment; ++OtherCompartment)
		{
			input += RESULT(KOutput, OtherCompartment) * PARAMETER(FlowFraction, OtherCompartment, ThisCompartment) * PARAMETER(RelativeArea, OtherCompartment);
		}
		return input / PARAMETER(RelativeArea);
	)
	
	EQUATION(Model, NH4Input,
		double input = 0.0;
		index_t ThisCompartment = CURRENT_INDEX(Compartment);
		for(index_t OtherCompartment = FIRST_INDEX(Compartment); OtherCompartment < ThisCompartment; ++OtherCompartment)
		{
			input += RESULT(NH4Output, OtherCompartment) * PARAMETER(FlowFraction, OtherCompartment, ThisCompartment) * PARAMETER(RelativeArea, OtherCompartment);
		}
		return input / PARAMETER(RelativeArea);
	)
	
	EQUATION(Model, SO4Input,
		double input = 0.0;
		index_t ThisCompartment = CURRENT_INDEX(Compartment);
		for(index_t OtherCompartment = FIRST_INDEX(Compartment); OtherCompartment < ThisCompartment; ++OtherCompartment)
		{
			input += RESULT(SO4Output, OtherCompartment) * PARAMETER(FlowFraction, OtherCompartment, ThisCompartment) * PARAMETER(RelativeArea, OtherCompartment);
		}
		return input / PARAMETER(RelativeArea);
	)
	
	EQUATION(Model, ClInput,
		double input = 0.0;
		index_t ThisCompartment = CURRENT_INDEX(Compartment);
		for(index_t OtherCompartment = FIRST_INDEX(Compartment); OtherCompartment < ThisCompartment; ++OtherCompartment)
		{
			input += RESULT(ClOutput, OtherCompartment) * PARAMETER(FlowFraction, OtherCompartment, ThisCompartment) * PARAMETER(RelativeArea, OtherCompartment);
		}
		return input / PARAMETER(RelativeArea);
	)
	
	EQUATION(Model, NO3Input,
		double input = 0.0;
		index_t ThisCompartment = CURRENT_INDEX(Compartment);
		for(index_t OtherCompartment = FIRST_INDEX(Compartment); OtherCompartment < ThisCompartment; ++OtherCompartment)
		{
			input += RESULT(NO3Output, OtherCompartment) * PARAMETER(FlowFraction, OtherCompartment, ThisCompartment) * PARAMETER(RelativeArea, OtherCompartment);
		}
		return input / PARAMETER(RelativeArea);
	)
	
	EQUATION(Model, FInput,
		double input = 0.0;
		index_t ThisCompartment = CURRENT_INDEX(Compartment);
		for(index_t OtherCompartment = FIRST_INDEX(Compartment); OtherCompartment < ThisCompartment; ++OtherCompartment)
		{
			input += RESULT(FOutput, OtherCompartment) * PARAMETER(FlowFraction, OtherCompartment, ThisCompartment) * PARAMETER(RelativeArea, OtherCompartment);
		}
		return input / PARAMETER(RelativeArea);
	)
	
	EQUATION(Model, PO4Input,
		double input = 0.0;
		index_t ThisCompartment = CURRENT_INDEX(Compartment);
		for(index_t OtherCompartment = FIRST_INDEX(Compartment); OtherCompartment < ThisCompartment; ++OtherCompartment)
		{
			input += RESULT(PO4Output, OtherCompartment) * PARAMETER(FlowFraction, OtherCompartment, ThisCompartment) * PARAMETER(RelativeArea, OtherCompartment);
		}
		return input / PARAMETER(RelativeArea);
	)
	
	EQUATION(Model, TotalCa,
		return RESULT(CaExternalFlux) - RESULT(CaOutput) + RESULT(CaInput);
	)
	
	EQUATION(Model, TotalMg,
		return RESULT(MgExternalFlux) - RESULT(MgOutput) + RESULT(MgInput);
	)
	
	EQUATION(Model, TotalNa,
		return RESULT(NaExternalFlux) - RESULT(NaOutput) + RESULT(NaInput);
	)
	
	EQUATION(Model, TotalK,
		return RESULT(KExternalFlux)  - RESULT(KOutput) + RESULT(KInput);
	)
	
	EQUATION(Model, TotalNH4,
		return RESULT(NH4ExternalFlux) - RESULT(NH4Output) + RESULT(NH4Input);
	)
	
	EQUATION(Model, TotalSO4,
		return RESULT(SO4ExternalFlux) - RESULT(SO4Output) + RESULT(SO4Input);
	)
	
	EQUATION(Model, TotalCl,
		return RESULT(ClExternalFlux) - RESULT(ClOutput) + RESULT(ClInput);
	)
	
	EQUATION(Model, TotalNO3,
		return RESULT(NO3ExternalFlux) - RESULT(NO3Output) + RESULT(NO3Input);
	)
	
	EQUATION(Model, TotalF,
		return RESULT(FExternalFlux) - RESULT(FOutput) + RESULT(FInput);
	)
	
	EQUATION(Model, TotalPO4,
		return RESULT(PO4ExternalFlux) - RESULT(PO4Output) + RESULT(PO4Input);
	)
	
	EQUATION(Model, CationExchangeCapacityEq,        // NOTE: We need this one so that it can be overridden in the MAGIC Forest CNP module.
		return PARAMETER(CationExchangeCapacity);
	)
	
	EQUATION(Model, ConcH,
		magic_input Input;
		magic_param Param;
		magic_output Result;
		
		Input.total_Ca    = RESULT(TotalCa);
		Input.total_Mg    = RESULT(TotalMg);
		Input.total_Na    = RESULT(TotalNa);
		Input.total_K     = RESULT(TotalK);
		Input.total_NH4   = RESULT(TotalNH4);
		Input.total_SO4   = RESULT(TotalSO4);
		Input.total_Cl    = RESULT(TotalCl);
		Input.total_NO3   = RESULT(TotalNO3);
		Input.total_F     = RESULT(TotalF);
		Input.total_PO4   = RESULT(TotalPO4);
		
		Param.Depth       = PARAMETER(Depth);
		Param.Temperature = RESULT(Temperature);
		Param.PartialPressureCO2 = RESULT(PartialPressureCO2);
		Param.conc_DOC    = RESULT(OAConcentration);
		
		Param.Log10AlOH3EquilibriumConst = PARAMETER(Log10AlOH3EquilibriumConst);
		Param.HAlOH3Exponent             = PARAMETER(HAlOH3Exponent);
		Param.pK1DOC                     = PARAMETER(PK1DOC);
		Param.pK2DOC                     = PARAMETER(PK2DOC);
		Param.pK3DOC                     = PARAMETER(PK3DOC);
		Param.pK1AlDOC                   = PARAMETER(PK1AlDOC);
		Param.pK2AlDOC                   = PARAMETER(PK2AlDOC);
		
		Param.Porosity                   = PARAMETER(Porosity);
		Param.BulkDensity                = PARAMETER(BulkDensity);
		//Param.CationExchangeCapacity     = PARAMETER(CationExchangeCapacity);
		Param.CationExchangeCapacity     = RESULT(CationExchangeCapacityEq);
		Param.SO4HalfSat                 = PARAMETER(SO4HalfSat);
		Param.SO4MaxCap                  = PARAMETER(SO4MaxCap);
		
		Param.Log10CaAlSelectCoeff       = RESULT(Log10CaAlSelectCoeff);
		Param.Log10MgAlSelectCoeff       = RESULT(Log10MgAlSelectCoeff);
		Param.Log10NaAlSelectCoeff       = RESULT(Log10NaAlSelectCoeff);
		Param.Log10KAlSelectCoeff        = RESULT(Log10KAlSelectCoeff);
		
		bool   issoil     = PARAMETER(IsSoil);
		double conv       = PARAMETER(ConvergenceCriterion);
		double H_estimate = LAST_RESULT(ConcH);
		double ionic      = LAST_RESULT(IonicStrength);
		
		if(RunState__->Running)   // Safeguard so that we don't crash on initialisation run.
		{
			MagicCore(Input, Param, Result, issoil, conv, H_estimate, ionic, CURRENT_TIMESTEP(), (s64)CURRENT_INDEX(Compartment).Index);
		}
		
		SET_RESULT(ConcCa,            2.0*Result.conc_Ca);
		SET_RESULT(ConcMg,            2.0*Result.conc_Mg);
		SET_RESULT(ConcNa,            Result.conc_Na);
		SET_RESULT(ConcK,             Result.conc_K);
		SET_RESULT(ConcNH4,           Result.conc_NH4);
		SET_RESULT(ConcSO4,           2.0*Result.conc_SO4);
		SET_RESULT(ConcCl,            Result.conc_Cl);
		SET_RESULT(ConcNO3,           Result.conc_NO3);
		SET_RESULT(ConcF,             Result.conc_F);
		SET_RESULT(ConcPO4,           3.0*Result.conc_PO4);    //TODO: Check if the 3.0 is correct!
		
		SET_RESULT(ConcAllSO4,        2.0*Result.all_SO4);
		SET_RESULT(ConcAllF,          Result.all_F);
		
		SET_RESULT(IonicStrength,     Result.IonicStrength);
		
		SET_RESULT(PH,                Result.pH);
		SET_RESULT(SumBaseCationConc, Result.SumBaseCationConc);
		SET_RESULT(SumAcidAnionConc,  Result.SumAcidAnionConc);
		SET_RESULT(ChargeBalanceAlk,  Result.ChargeBalanceAlk);
		SET_RESULT(WeakAcidAlk,       Result.WeakAcidAlk);
		
		SET_RESULT(ExchangeableCa,    100.0*Result.exchangeable_Ca);
		SET_RESULT(ExchangeableMg,    100.0*Result.exchangeable_Mg);
		SET_RESULT(ExchangeableNa,    100.0*Result.exchangeable_Na);
		SET_RESULT(ExchangeableK,     100.0*Result.exchangeable_K);
		SET_RESULT(ExchangeableSO4,   100.0*Result.exchangeable_SO4);
		SET_RESULT(BaseSaturationSoil,100.0*Result.BaseSaturationSoil);
		
		SET_RESULT(ConcHCO3,          Result.conc_HCO3);
		SET_RESULT(ConcCO3,           Result.conc_CO3);
		SET_RESULT(ConcAl,            Result.conc_Al);
		SET_RESULT(ConcAllAl,         Result.all_Al);
		SET_RESULT(ConcOrgAl,         Result.org_Al);
		
		SET_RESULT(ConcH2AM,          Result.conc_H2AM);
		SET_RESULT(ConcHA2M,          Result.conc_HA2M);
		SET_RESULT(ConcA3M,           Result.conc_A3M);
		
		SET_RESULT(SumPositive,       Result.SumPositive);
		SET_RESULT(SumNegative,       Result.SumNegative);

		SET_RESULT(ConcAllDOC,        Result.all_DOC);
		SET_RESULT(ConcAllDIC,        Result.all_DIC);
		SET_RESULT(CaAlRatio,         Result.CaAlRatio);
		
		return Result.conc_H;
	)
	
	EquationIsComputedBy(Model, ConcCa,  ConcH, Compartment);
	EquationIsComputedBy(Model, ConcMg,  ConcH, Compartment);
	EquationIsComputedBy(Model, ConcNa,  ConcH, Compartment);
	EquationIsComputedBy(Model, ConcK,   ConcH, Compartment);
	EquationIsComputedBy(Model, ConcNH4, ConcH, Compartment);
	EquationIsComputedBy(Model, ConcSO4, ConcH, Compartment);
	EquationIsComputedBy(Model, ConcAllSO4, ConcH, Compartment);
	EquationIsComputedBy(Model, ConcCl,  ConcH, Compartment);
	EquationIsComputedBy(Model, ConcNO3, ConcH, Compartment);
	EquationIsComputedBy(Model, ConcF,   ConcH, Compartment);
	EquationIsComputedBy(Model, ConcAllF, ConcH, Compartment);
	EquationIsComputedBy(Model, ConcPO4, ConcH, Compartment);
	EquationIsComputedBy(Model, PH,  ConcH, Compartment);
	EquationIsComputedBy(Model, SumBaseCationConc, ConcH, Compartment);
	EquationIsComputedBy(Model, SumAcidAnionConc, ConcH, Compartment);
	EquationIsComputedBy(Model, ChargeBalanceAlk, ConcH, Compartment);
	EquationIsComputedBy(Model, WeakAcidAlk, ConcH, Compartment);
	EquationIsComputedBy(Model, ExchangeableCa, ConcH, Compartment);
	EquationIsComputedBy(Model, ExchangeableMg, ConcH, Compartment);
	EquationIsComputedBy(Model, ExchangeableNa, ConcH, Compartment);
	EquationIsComputedBy(Model, ExchangeableK, ConcH, Compartment);
	EquationIsComputedBy(Model, ExchangeableSO4, ConcH, Compartment);
	EquationIsComputedBy(Model, BaseSaturationSoil, ConcH, Compartment);
	EquationIsComputedBy(Model, ConcHCO3, ConcH, Compartment);
	EquationIsComputedBy(Model, ConcCO3, ConcH, Compartment);
	EquationIsComputedBy(Model, ConcAl, ConcH, Compartment);
	EquationIsComputedBy(Model, ConcAllAl, ConcH, Compartment);
	EquationIsComputedBy(Model, ConcOrgAl, ConcH, Compartment);
	EquationIsComputedBy(Model, ConcH2AM, ConcH, Compartment);
	EquationIsComputedBy(Model, ConcHA2M, ConcH, Compartment);
	EquationIsComputedBy(Model, ConcA3M, ConcH, Compartment);
	EquationIsComputedBy(Model, SumPositive, ConcH, Compartment);
	EquationIsComputedBy(Model, SumNegative, ConcH, Compartment);
	EquationIsComputedBy(Model, ConcAllDOC, ConcH, Compartment);
	EquationIsComputedBy(Model, ConcAllDIC, ConcH, Compartment);
	EquationIsComputedBy(Model, CaAlRatio, ConcH, Compartment);
	EquationIsComputedBy(Model, IonicStrength, ConcH, Compartment);

	EndModule(Model);
}