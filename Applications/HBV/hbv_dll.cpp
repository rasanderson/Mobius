

#define MOBIUS_TIMESTEP_VERBOSITY 0
#define MOBIUS_TEST_FOR_NAN 0
#define MOBIUS_EQUATION_PROFILING 0
#define MOBIUS_PRINT_TIMING_INFO 0

#include "../python_wrapper.h"

#include "../../Modules/HBV.h"



DLLEXPORT void *
DllSetupModel(char *ParameterFilename, char *InputFilename) {
    
	CHECK_ERROR_BEGIN
	
	mobius_model *Model = BeginModelDefinition("HBV", "1.0");
	
	AddHBVModel(Model);
	
	ReadInputDependenciesFromFile(Model, InputFilename);
	
	EndModelDefinition(Model);
	
	mobius_data_set *DataSet = GenerateDataSet(Model);
	
	ReadParametersFromFile(DataSet, ParameterFilename);
	ReadInputsFromFile(DataSet, InputFilename);
	
	return (void *)DataSet;
	
	CHECK_ERROR_END
	
	return 0;
}