#define MOBIUS_TIMESTEP_VERBOSITY 0
#define MOBIUS_TEST_FOR_NAN 0
#define MOBIUS_EQUATION_PROFILING 0
#define MOBIUS_PRINT_TIMING_INFO 0

#include "../../mobius_dll.h"

#include "../../Modules/UnitConversions.h"

//#define SIMPLYQ_GROUNDWATER    //NOTE: #define this before the inclusion of the SimplyQ.h file if you want SimplyQ to simulate groundwater
								 //Comment out this line if you don't want groundwater
#include "../../Modules/SimplyQ.h"

#include "../../Modules/SimplyC.h"
#include "../../Modules/SimplySoilTemperature.h"

DLLEXPORT void *
DllSetupModel(char *ParameterFilename, char *InputFilename)
{
	CHECK_ERROR_BEGIN
	
	mobius_model *Model = BeginModelDefinition("SimplyC", "0.1");
	
	AddSimplyHydrologyModule(Model);
	AddSoilTemperatureModel(Model);
	AddSimplyCModel(Model);
	
	ReadInputDependenciesFromFile(Model, InputFilename);
	
	EndModelDefinition(Model);
	
	mobius_data_set *DataSet = GenerateDataSet(Model);
	
	ReadParametersFromFile(DataSet, ParameterFilename);
	ReadInputsFromFile(DataSet, InputFilename);
	
	return (void *)DataSet;
	
	CHECK_ERROR_END
	
	return 0;
}