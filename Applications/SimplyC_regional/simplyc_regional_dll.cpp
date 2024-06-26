#define MOBIUS_TIMESTEP_VERBOSITY 0
#define MOBIUS_TEST_FOR_NAN 0
#define MOBIUS_EQUATION_PROFILING 0
#define MOBIUS_PRINT_TIMING_INFO 0

#include "../../mobius_dll.h"

#include "../../Modules/SolarRadiation.h"
#include "../../Modules/PET.h"
          
#include "../../Modules/Simply/SimplySnow.h"
#define SIMPLYQ_GROUNDWATER
#include "../../Modules/Simply/SimplyQ.h"
//#include "../../Modules/Simply/SimplyQ_ballycanew.h"
#include "../../Modules/Simply/SimplyC.h"

#include "../../Modules/SimplySoilTemperature.h"

//#include "../../Modules/Simply/SimplySed.h"
#include "../../Modules/EasyLake/SuperEasyLake.h"


mobius_model *
DllBuildModel()
{
	mobius_model *Model = BeginModelDefinition("SimplyC (regional)", true);
	
	AddMaxSolarRadiationModule(Model);
	AddPriestleyTaylorPETModule2(Model);
	AddSimplySnowModule(Model);
	AddSimplyHydrologyModule(Model);
	AddSoilTemperatureModel(Model);
	//AddSimplyCModel(Model);
	
	AddSuperEasyLakeModule(Model);
	
	//AddAwfullySimplySedimentModule(Model);
	AddSimplyCModel(Model);
	//AddSimplyTOCModule(Model);
	
	AddSuperEasyLakeCModule(Model);
	
	return Model;
}
