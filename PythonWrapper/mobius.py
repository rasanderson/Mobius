import ctypes
import numpy as np
import datetime as dt
import warnings

mobiusdll = None

class TimestepSize(ctypes.Structure):
	_fields_ = [("type", ctypes.c_int), ("magnitude", ctypes.c_int)]
	
class dll_branch_index(ctypes.Structure):
	_fields_ = [("IndexName", ctypes.c_char_p), ("BranchCount", ctypes.c_uint64), ("BranchNames", ctypes.POINTER(ctypes.c_char_p))]

def initialize(dllname) :
	global mobiusdll
	mobiusdll = ctypes.CDLL(dllname)

	
	mobiusdll.DllEncounteredError.argtypes = [ctypes.c_char_p]
	mobiusdll.DllEncounteredError.restype = ctypes.c_int
	
	mobiusdll.DllEncounteredWarning.argtypes = [ctypes.c_char_p]
	mobiusdll.DllEncounteredWarning.restype = ctypes.c_int
	
	mobiusdll.DllSetupModel.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
	mobiusdll.DllSetupModel.restype  = ctypes.c_void_p
	
	mobiusdll.DllSetupModelBlankIndexSets.argtypes = [ctypes.c_char_p]
	mobiusdll.DllSetupModelBlankIndexSets.restype  = ctypes.c_void_p
	
	mobiusdll.DllReadInputs.argtypes = [ctypes.c_void_p, ctypes.c_char_p]
	
	mobiusdll.DllReadParameters.argtypes = [ctypes.c_void_p, ctypes.c_char_p]
	
	mobiusdll.DllSetIndexes.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_uint64, ctypes.POINTER(ctypes.c_char_p)]
		
	mobiusdll.DllSetBranchIndexes.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_uint64, ctypes.POINTER(dll_branch_index)]

	mobiusdll.DllRunModel.argtypes = [ctypes.c_void_p]

	mobiusdll.DllCopyDataSet.argtypes = [ctypes.c_void_p, ctypes.c_bool]
	mobiusdll.DllCopyDataSet.restype  = ctypes.c_void_p

	mobiusdll.DllDeleteDataSet.argtypes = [ctypes.c_void_p]

	mobiusdll.DllGetTimesteps.argtypes = [ctypes.c_void_p]
	mobiusdll.DllGetTimesteps.restype = ctypes.c_uint64
	
	mobiusdll.DllGetTimestepSize.argtypes = [ctypes.c_void_p]
	mobiusdll.DllGetTimestepSize.restype  = TimestepSize

	mobiusdll.DllGetInputTimesteps.argtypes = [ctypes.c_void_p]
	mobiusdll.DllGetInputTimesteps.restype = ctypes.c_uint64

	mobiusdll.DllGetInputStartDate.argtypes = [ctypes.c_void_p, ctypes.c_char_p]

	mobiusdll.DllGetResultSeries.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p), ctypes.c_uint64, ctypes.POINTER(ctypes.c_double)]

	mobiusdll.DllGetInputSeries.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p), ctypes.c_uint64, ctypes.POINTER(ctypes.c_double), ctypes.c_bool]

	mobiusdll.DllSetParameterDouble.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p), ctypes.c_uint64, ctypes.c_double]

	mobiusdll.DllSetParameterUInt.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p), ctypes.c_uint64, ctypes.c_uint64]

	mobiusdll.DllSetParameterBool.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p), ctypes.c_uint64, ctypes.c_bool]

	mobiusdll.DllSetParameterTime.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p), ctypes.c_uint64, ctypes.c_char_p]

	mobiusdll.DllWriteParametersToFile.argtypes = [ctypes.c_void_p, ctypes.c_char_p]

	mobiusdll.DllGetParameterDouble.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p), ctypes.c_uint64]
	mobiusdll.DllGetParameterDouble.restype = ctypes.c_double

	mobiusdll.DllGetParameterUInt.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p), ctypes.c_uint64]
	mobiusdll.DllGetParameterUInt.restype  = ctypes.c_uint64

	mobiusdll.DllGetParameterBool.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p), ctypes.c_uint64]
	mobiusdll.DllGetParameterBool.restype  = ctypes.c_bool

	mobiusdll.DllGetParameterTime.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p), ctypes.c_uint64, ctypes.c_char_p]

	mobiusdll.DllSetInputSeries.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p), ctypes.c_uint64, ctypes.POINTER(ctypes.c_double), ctypes.c_uint64, ctypes.c_bool]
	
	mobiusdll.DllGetIndexSetsCount.argtypes = [ctypes.c_void_p]
	mobiusdll.DllGetIndexSetsCount.restype = ctypes.c_uint64
	
	mobiusdll.DllGetIndexSets.argtypes = [ctypes.c_void_p, ctypes.POINTER(ctypes.c_char_p), ctypes.POINTER(ctypes.c_char_p)]

	mobiusdll.DllGetIndexCount.argtypes = [ctypes.c_void_p, ctypes.c_char_p]
	mobiusdll.DllGetIndexCount.restype = ctypes.c_uint64
	
	mobiusdll.DllGetIndexes.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p)]
	
	mobiusdll.DllGetParameterIndexSetsCount.argtypes = [ctypes.c_void_p, ctypes.c_char_p]
	mobiusdll.DllGetParameterIndexSetsCount.restype = ctypes.c_uint64
	
	mobiusdll.DllGetParameterIndexSets.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p)]
	
	mobiusdll.DllGetResultIndexSetsCount.argtypes = [ctypes.c_void_p, ctypes.c_char_p]
	mobiusdll.DllGetResultIndexSetsCount.restype  = ctypes.c_uint64
	
	mobiusdll.DllGetResultIndexSets.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p)]
	
	mobiusdll.DllGetInputIndexSetsCount.argtypes = [ctypes.c_void_p, ctypes.c_char_p]
	mobiusdll.DllGetInputIndexSetsCount.restype  = ctypes.c_uint64
	
	mobiusdll.DllGetInputIndexSets.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p)]
	
	mobiusdll.DllGetParameterDoubleMinMax.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_double), ctypes.POINTER(ctypes.c_double)]
	
	mobiusdll.DllGetParameterUIntMinMax.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_uint64), ctypes.POINTER(ctypes.c_uint64)]
	
	mobiusdll.DllGetParameterDescription.argtypes = [ctypes.c_void_p, ctypes.c_char_p]
	mobiusdll.DllGetParameterDescription.restype = ctypes.c_char_p
	
	mobiusdll.DllGetParameterShortName.argtypes = [ctypes.c_void_p, ctypes.c_char_p]
	mobiusdll.DllGetParameterShortName.restype = ctypes.c_char_p
	
	mobiusdll.DllGetParameterUnit.argtypes = [ctypes.c_void_p, ctypes.c_char_p]
	mobiusdll.DllGetParameterUnit.restype = ctypes.c_char_p
	
	mobiusdll.DllGetResultUnit.argtypes = [ctypes.c_void_p, ctypes.c_char_p]
	mobiusdll.DllGetResultUnit.restype = ctypes.c_char_p
	
	mobiusdll.DllGetAllParametersCount.argtypes = [ctypes.c_void_p, ctypes.c_char_p]
	mobiusdll.DllGetAllParametersCount.restype = ctypes.c_uint64
	
	mobiusdll.DllGetAllParameters.argtypes = [ctypes.c_void_p, ctypes.POINTER(ctypes.c_char_p), ctypes.POINTER(ctypes.c_char_p), ctypes.c_char_p]
	
	mobiusdll.DllGetAllResultsCount.argtypes = [ctypes.c_void_p, ctypes.c_char_p]
	mobiusdll.DllGetAllResultsCount.restype = ctypes.c_uint64
	
	mobiusdll.DllGetAllResults.argtypes = [ctypes.c_void_p, ctypes.POINTER(ctypes.c_char_p), ctypes.POINTER(ctypes.c_char_p), ctypes.c_char_p]
	
	mobiusdll.DllGetAllInputsCount.argtypes = [ctypes.c_void_p]
	mobiusdll.DllGetAllInputsCount.restype = ctypes.c_uint64
	
	mobiusdll.DllGetAllInputs.argtypes = [ctypes.c_void_p, ctypes.POINTER(ctypes.c_char_p), ctypes.POINTER(ctypes.c_char_p)]
	
	mobiusdll.DllInputWasProvided.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p), ctypes.c_uint64]
	mobiusdll.DllInputWasProvided.restype = ctypes.c_bool
	
	mobiusdll.DllGetBranchInputsCount.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_char_p]
	mobiusdll.DllGetBranchInputsCount.restype = ctypes.c_uint64
	
	mobiusdll.DllGetBranchInputs.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p)]

def _CStr(string):
	return string.encode('utf-8')   #TODO: We should figure out what encoding is best to use here.

def _PackIndexes(indexes):
	cindexes = [index.encode('utf-8') for index in indexes]
	return (ctypes.c_char_p * len(cindexes))(*cindexes)
	
def check_dll_error() :
	msgbuf = ctypes.create_string_buffer(1024)
	
	warncode  = mobiusdll.DllEncounteredWarning(msgbuf)
	if warncode == 1 :
		warnmsg = msgbuf.value.decode('utf-8')
		#warnings.warn(warnmsg)
		print(warnmsg)

	errcode = mobiusdll.DllEncounteredError(msgbuf)
	if errcode == 1 :
		errmsg = msgbuf.value.decode('utf-8')
		raise RuntimeError(errmsg)
	
class DataSet :
	def __init__(self, datasetptr):
		self.datasetptr = datasetptr
	
	@classmethod
	def setup_from_parameter_and_input_files(cls, parameterfilename, inputfilename) :
		'''
		Set up a complete dataset from the given parameter and input files
		'''
		datasetptr = mobiusdll.DllSetupModel(_CStr(parameterfilename), _CStr(inputfilename))
		check_dll_error()
		return cls(datasetptr)
		
	@classmethod
	def setup_with_blank_index_sets(cls, inputfilename) :
		'''
		Set up an incomplete dataset. The input file is only provided to structure the index set dependencies of inputs, and is not used to read in input data at this point. The dataset can not be used before all index sets are set either using set_indexes and set_indexes_branched, and/or read_parameters
		'''
		datasetptr = mobiusdll.DllSetupModelBlankIndexSets(_CStr(inputfilename))
		check_dll_error()
		return cls(datasetptr)
	
	def set_indexes(self, index_set, indexes):
		'''
		Set the indexes of an index set. This can only be done on an incomplete dataset, and only on an index set that has not yet received indexes.
		
		Arguments:
				index_set        -- string. The name of the index set. Example : "Lanscape units"
				indexes          -- list of strings.
		'''
		mobiusdll.DllSetIndexes(self.datasetptr, _CStr(index_set), len(indexes), _PackIndexes(indexes))
		check_dll_error()
		
	def set_branch_indexes(self, index_set, indexes):
		'''
		Set the indexes of a branched index set. This can only be done on an incomplete dataset, and only on an index set that has not yet received indexes.
		
		Arguments:
				index_set        -- string. The name of the index set. Example : "Reaches"
				indexes          -- list of pairs (index, inputs), where index is the (string) name of a new index, and inputs is a list of strings containing other names of input branches to this index.
		'''
		index_data = (dll_branch_index * len(indexes))()
		for idx in range(len(indexes)) :
			name, branch_inputs = indexes[idx]
			index_data[idx].IndexName = _CStr(name)
			index_data[idx].BranchCount = len(branch_inputs)
			index_data[idx].BranchNames = _PackIndexes(branch_inputs)
		mobiusdll.DllSetBranchIndexes(self.datasetptr, _CStr(index_set), len(indexes), index_data)
		check_dll_error()
	
	def read_inputs(self, inputfilename) :
		'''
		Read in input data into an incomplete dataset.
		'''
		mobiusdll.DllReadInputs(self.datasetptr, _CStr(inputfilename))
		check_dll_error()
		
	def read_parameters(self, parfilename) :
		'''
		Read in index set and parameter data into an incomplete dataset. Any index sets that are not given indexes in this parameter file has to have been given indexes prior to this call using set_indexes and/or set_indexes_branched.
		'''
		mobiusdll.DllReadParameters(self.datasetptr, _CStr(parfilename))
		check_dll_error()
		
	def run_model(self) :
		'''
		Runs the model with the parameters and input series that are currently stored in the dataset. All result series will also be stored in the dataset.
		'''
		mobiusdll.DllRunModel(self.datasetptr)
		check_dll_error()
	
	def copy(self, copyresults=False) :
		'''
		Create a copy of the dataset that contains all the same parameter values and input series. Result series will not be copied.
		'''
		cp = DataSet(mobiusdll.DllCopyDataSet(self.datasetptr, copyresults))
		check_dll_error()
		return cp
		
	def delete(self) :
		'''
		Delete all data that was allocated by the C++ code for this dataset. Interaction with the dataset after it was deleted is not recommended. Note that this will not delete the model itself, only the parameter, input and result data. This is because typically you can have multiple datasets sharing the same model (such as if you created dataset copies using dataset.copy()). There is currently no way to delete the model.
		'''
		mobiusdll.DllDeleteDataSet(self.datasetptr)
		check_dll_error()
		
	def write_parameters_to_file(self, filename) :
		'''
		Write the parameters in the dataset to a standard Mobius parameter file.
		'''
		mobiusdll.DllWriteParametersToFile(self.datasetptr, _CStr(filename))
		check_dll_error()
		
	def get_timestep_size(self) :
		'''
		Get the size of the timestep of the model. Returned as a pair (type, magnitude), where type is either 'S' (second) or 'MS' (month).
		'''
		timestep = mobiusdll.DllGetTimestepSize(self.datasetptr)
		return ('S' if timestep.type == 0 else 'M', timestep.magnitude)
		
	def get_result_series(self, name, indexes) :
		'''
		Extract one of the result series that was produced by the model. Can only be called after dataset.run_model() has been called at least once.
		
		Arguments:
			name             -- string. The name of the result series. Example : "Soil moisture"
			indexes          -- list of strings. A list of index names to identify the particular input series. Example : ["Langtjern"] or ["Langtjern", "Forest"]
		
		Returns:
			A numpy.array containing the specified timeseries.
		'''
		timesteps = mobiusdll.DllGetTimesteps(self.datasetptr)
		check_dll_error()
	
		resultseries = (ctypes.c_double * timesteps)()
	
		mobiusdll.DllGetResultSeries(self.datasetptr, _CStr(name), _PackIndexes(indexes), len(indexes), resultseries)
		check_dll_error()
	
		return np.array(resultseries, copy=False)
		
	def get_input_series(self, name, indexes, alignwithresults=False) :
		'''
		Extract one of the input series that were provided with the dataset.
		
		Arguments:
			name             -- string. The name of the input series. Example : "Air temperature"
			indexes          -- list of strings. A list of index names to identify the particular input series. Example : ["Langtjern"] or ["Langtjern", "Forest"]
			alignwithresults -- boolean. If False: Extract the entire input series that was provided in the input file. If True: Extract the series from the parameter 'Start date', with 'Timesteps' number of values (i.e. aligned with any result series of the dataset).
		
		Returns:
			A numpy.array containing the specified timeseries.
		'''
		if alignwithresults :
			#timesteps = mobiusdll.DllGetTimesteps(self.datasetptr)
			timesteps = self.get_parameter_uint('Timesteps', [])    #NOTE: DllGetTimesteps returns TimestepsLastRun, so can not be used unless model has been run!!
		else :
			timesteps = mobiusdll.DllGetInputTimesteps(self.datasetptr)
		check_dll_error()
		
		inputseries = (ctypes.c_double * timesteps)()
		
		mobiusdll.DllGetInputSeries(self.datasetptr, _CStr(name), _PackIndexes(indexes), len(indexes), inputseries, alignwithresults)
		check_dll_error()
		
		return np.array(inputseries, copy=False)
		
	def set_input_series(self, name, indexes, inputseries, alignwithresults=False) :
		'''
		Overwrite one of the input series in the dataset.
		
		Arguments:
			name             -- string. The name of the input series. Example : "Air temperature"
			indexes          -- list of strings. A list of index names to identify the particular input series. Example : ["Langtjern"] or ["Langtjern", "Forest"]
			inputseries      -- list of double. The values to set.
			alignwithresults -- boolean. If False: Start writing to the first timestep of the input series. If True: Start writing at the timestep corresponding to the parameter 'Start date'.
		'''
		array = (ctypes.c_double * len(inputseries))(*inputseries)
		
		mobiusdll.DllSetInputSeries(self.datasetptr, _CStr(name), _PackIndexes(indexes), len(indexes), array, len(inputseries), alignwithresults)
		check_dll_error()
	
	def set_parameter_double(self, name, indexes, value):
		'''
		Overwrite the value of one parameter. Can only be called on parameters that were registered with the type double.
		
		Arguments:
			name             -- string. The name of the parameter. Example : "Maximum capacity"
			indexes          -- list of strings. A list of index names to identify the particular parameter instance. Example : [], ["Langtjern"] or ["Langtjern", "Forest"]
			value            -- float. The value to write to the parameter.
		'''
		mobiusdll.DllSetParameterDouble(self.datasetptr, _CStr(name), _PackIndexes(indexes), len(indexes), ctypes.c_double(value))
		check_dll_error()
	
	def set_parameter_uint(self, name, indexes, value):
		'''
		Overwrite the value of one parameter. Can only be called on parameters that were registered with the type uint.
		
		Arguments:
			name             -- string. The name of the parameter. Example : "Fertilizer addition start day"
			indexes          -- list of strings. A list of index names to identify the particular parameter instance. Example : [], ["Langtjern"] or ["Langtjern", "Forest"]
			value            -- unsigned integer. The value to write to the parameter.
		'''
		mobiusdll.DllSetParameterUInt(self.datasetptr, _CStr(name), _PackIndexes(indexes), len(indexes), ctypes.c_uint64(value))
		check_dll_error()
		
	def set_parameter_bool(self, name, indexes, value):
		'''
		Overwrite the value of one parameter. Can only be called on parameters that were registered with the type bool.
		
		Arguments:
			name             -- string. The name of the parameter. Example : "Reach has effluent inputs"
			indexes          -- list of strings. A list of index names to identify the particular parameter instance. Example : [], ["Langtjern"] or ["Langtjern", "Forest"]
			value            -- bool. The value to write to the parameter.
		'''
		mobiusdll.DllSetParameterBool(self.datasetptr, _CStr(name), _PackIndexes(indexes), len(indexes), ctypes.c_bool(value))
		check_dll_error()
		
	def set_parameter_time(self, name, indexes, value):
		'''
		Overwrite the value of one parameter. Can only be called on parameters that were registered with the type time.
		
		Arguments:
			name             -- string. The name of the parameter. Example : "Start date"
			indexes          -- list of strings. A list of index names to identify the particular parameter instance. Example : [], ["Langtjern"] or ["Langtjern", "Forest"]
			value            -- string or datetime. The value to write to the parameter. If provided as a string it must be on the format "YYYY-MM-dd", e.g. "1999-05-15"
		'''
		#TODO: we should probably also recognize datetime64 (used by pandas) and Timestamp value types.
		if type(value) == dt.datetime :
			strvalue = value.strftime('%Y-%m-%d')
		else :
			strvalue = value
		
		mobiusdll.DllSetParameterTime(self.datasetptr, _CStr(name), _PackIndexes(indexes), len(indexes), strvalue.encode('utf-8'))
		check_dll_error()
		
	def get_parameter_double(self, name, indexes) :
		'''
		Read the value of one parameter. Can only be called on parameters that were registered with the type double.
		
		Arguments:
			name             -- string. The name of the parameter. Example : "Maximum capacity"
			indexes          -- list of strings. A list of index names to identify the particular parameter instance. Example : [], ["Langtjern"] or ["Langtjern", "Forest"]
		
		Returns:
			The value of the parameter (a 64-bit floating point number).
		'''
		val = mobiusdll.DllGetParameterDouble(self.datasetptr, _CStr(name), _PackIndexes(indexes), len(indexes))
		check_dll_error()
		return val
		
	def get_parameter_uint(self, name, indexes) :
		'''
		Read the value of one parameter. Can only be called on parameters that were registered with the type uint.
		
		Arguments:
			name             -- string. The name of the parameter. Example : "Fertilizer addition start day"
			indexes          -- list of strings. A list of index names to identify the particular parameter instance. Example : [], ["Langtjern"] or ["Langtjern", "Forest"]
		
		Returns:
			The value of the parameter (a nonnegative integer).
		'''
		val = mobiusdll.DllGetParameterUInt(self.datasetptr, _CStr(name), _PackIndexes(indexes), len(indexes))
		check_dll_error()
		return val
		
	def get_parameter_bool(self, name, indexes) :
		'''
		Read the value of one parameter. Can only be called on parameters that were registered with the type bool.
		
		Arguments:
			name             -- string. The name of the parameter. Example : "Reach has effluent inputs"
			indexes          -- list of strings. A list of index names to identify the particular parameter instance. Example : [], ["Langtjern"] or ["Langtjern", "Forest"]
		
		Returns:
			The value of the parameter (a boolean).
		'''
		return mobiusdll.DllGetParameterBool(self.datasetptr, _CStr(name), _PackIndexes(indexes), len(indexes))
		
	def get_parameter_time(self, name, indexes) :
		'''
		Read the value of one parameter. Can only be called on parameters that were registered with the type time.
		
		Arguments:
			name             -- string. The name of the parameter. Example : "Start date"
			indexes          -- list of strings. A list of index names to identify the particular parameter instance. Example : [], ["Langtjern"] or ["Langtjern", "Forest"]
		
		Returns:
			The value of the parameter (as a datetime).
		'''

		string = ctypes.create_string_buffer(64)
		mobiusdll.DllGetParameterTime(self.datasetptr, _CStr(name), _PackIndexes(indexes), len(indexes), string)
		check_dll_error()
		datestr = string.value.decode('utf-8')
		try :
			result = dt.datetime.strptime(datestr, '%Y-%m-%d %H:%M:%S')
		except :
			result = dt.datetime.strptime(datestr, '%Y-%m-%d')
		return result
		
	def get_parameter_double_min_max(self, name) :
		'''
		Retrieve the recommended min and max values for a parameter. Can only be called on a parameter that was registered with type double.
		
		Arguments:
			name             -- string. The name of the parameter. Example : "Baseflow index"
			
		Returns:
			a tuple (min, max) where min and max are of type float
		'''
		min = ctypes.c_double(0)
		max = ctypes.c_double(0)
		mobiusdll.DllGetParameterDoubleMinMax(self.datasetptr, _CStr(name), ctypes.POINTER(ctypes.c_double)(min), ctypes.POINTER(ctypes.c_double)(max))
		check_dll_error()
		return (min.value, max.value)

	def get_parameter_uint_min_max(self, name):
		'''
		Retrieve the recommended min and max values for a parameter. Can only be called on a parameter that was registered with type uint.
		
		Arguments:
			name             -- string. The name of the parameter. Example : "Fertilizer addition period"
			
		Returns:
			a tuple (min, max) where min and max are of type nonzero integer
		'''
		min = ctypes.c_uint64(0)
		max = ctypes.c_uint64(0)
		mobiusdll.DllGetParameterUIntMinMax(self.datasetptr, _CStr(name), ctypes.POINTER(ctypes.c_uint64)(min), ctypes.POINTER(ctypes.c_uint64)(max))
		check_dll_error()
		return (min.value, max.value)
		
	def get_parameter_description(self, name):
		'''
		Retrieve (as a string) a longer form description of the parameter with the given name.
		'''
		desc = mobiusdll.DllGetParameterDescription(self.datasetptr, _CStr(name))
		check_dll_error()
		return '' if desc is None else desc.decode('utf-8') 
		
	def get_parameter_short_name(self, name):
		'''
		Retrieve (as a string) a short name (identifier) of the parameter with the given name
		'''
		sn = mobiusdll.DllGetParameterShortName(self.datasetptr, _CStr(name))
		check_dll_error()
		return '' if sn is None else sn.decode('utf-8')
		
	def get_parameter_unit(self, name):
		'''
		Retrieve (as a string) the unit of the parameter with the given name.
		'''
		unit = mobiusdll.DllGetParameterUnit(self.datasetptr, _CStr(name))
		check_dll_error()
		return unit.decode('utf-8')
		
	def get_result_unit(self, name) :
		'''
		Retrieve (as a string) the unit of the result series with the given name.
		'''
		unit = mobiusdll.DllGetResultUnit(self.datasetptr, _CStr(name))
		check_dll_error()
		return unit.decode('utf-8')
		
	def get_input_timesteps(self):
		'''
		Get the number of timesteps that was allocated for the input data.
		'''
		timesteps = mobiusdll.DllGetInputTimesteps(self.datasetptr)
		check_dll_error()
		return timesteps
		
	def get_input_start_date(self):
		'''
		Get the start date that was set for the input data. Returns a datetime.
		'''
		string = ctypes.create_string_buffer(32)
		mobiusdll.DllGetInputStartDate(self.datasetptr, string)
		check_dll_error()
		datestr = string.value.decode('utf-8')
		return dt.datetime.strptime(datestr, '%Y-%m-%d')
		
	def get_index_sets(self):
		'''
		Get the name of each index set that is present in the model. Returns a list of strings.
		'''
		num = mobiusdll.DllGetIndexSetsCount(self.datasetptr)
		check_dll_error()
		array = (ctypes.c_char_p * num)()
		typearray = (ctypes.c_char_p * num)()    #NOTE to not break the API, we don't return this. Maybe fix later?
		mobiusdll.DllGetIndexSets(self.datasetptr, array, typearray)
		check_dll_error()
		return [string.decode('utf-8') for string in array]
		
	def get_indexes(self, index_set):
		'''
		Get the name of each index in an index set.
		
		Arguments:
			index_set          -- string. The name of the index set
		
		Returns:
			A list of strings where each string is an index in the index set.
		'''
		num = mobiusdll.DllGetIndexCount(self.datasetptr, _CStr(index_set))
		check_dll_error()
		array = (ctypes.c_char_p * num)()
		mobiusdll.DllGetIndexes(self.datasetptr, _CStr(index_set), array)
		check_dll_error()
		return [string.decode('utf-8') for string in array]
		
	def get_parameter_index_sets(self, name):
		'''
		Get the name of each index set a parameter indexes over.
		
		Arguments:
			name            -- string. The name of the parameter
		
		Returns:
			A list of strings where each string is the name of an index set.
		'''
		num = mobiusdll.DllGetParameterIndexSetsCount(self.datasetptr, _CStr(name))
		check_dll_error()
		array = (ctypes.c_char_p * num)()
		mobiusdll.DllGetParameterIndexSets(self.datasetptr, _CStr(name), array)
		check_dll_error()
		return [string.decode('utf-8') for string in array]
		
	def get_result_index_sets(self, name):
		'''
		Get the name of each index set a result indexes over.
		
		Arguments:
			name            -- string. The name of the result series
		
		Returns:
			A list of strings where each string is the name of an index set.
		'''
		num = mobiusdll.DllGetResultIndexSetsCount(self.datasetptr, _CStr(name))
		check_dll_error()
		array = (ctypes.c_char_p * num)()
		mobiusdll.DllGetResultIndexSets(self.datasetptr, _CStr(name), array)
		check_dll_error()
		return [string.decode('utf-8') for string in array]
		
	def get_input_index_sets(self, name):
		'''
		Get the name of each index set an input series indexes over.
		
		Arguments:
			name            -- string. The name of the input series
		
		Returns:
			A list of strings where each string is the name of an index set.
		'''
		num = mobiusdll.DllGetInputIndexSetsCount(self.datasetptr, _CStr(name))
		check_dll_error()
		array = (ctypes.c_char_p * num)()
		mobiusdll.DllGetResultInputSets(self.datasetptr, _CStr(name), array)
		check_dll_error()
		return [string.decode('utf-8') for string in array]
		
	def get_parameter_list(self, groupname = '') :
		'''
		Get the name and type of all the parameters in the model as a list of pairs of strings.
		
		Arguments:
			groupname       -- string. (optional) Only list the parameters belonging to this parameter group.
		'''
		num = mobiusdll.DllGetAllParametersCount(self.datasetptr, _CStr(groupname))
		check_dll_error()
		namearray = (ctypes.c_char_p * num)()
		typearray = (ctypes.c_char_p * num)()
		mobiusdll.DllGetAllParameters(self.datasetptr, namearray, typearray, _CStr(groupname))
		check_dll_error()
		return [(name.decode('utf-8'), type.decode('utf-8')) for name, type in zip(namearray, typearray)]
		
	def get_equation_list(self) :
		'''
		Get the name and type of all the equations in the model as a list of pairs of strings.
		'''
		num = mobiusdll.DllGetAllResultsCount(self.datasetptr, '__all!!__')
		check_dll_error()
		namearray = (ctypes.c_char_p * num)()
		typearray = (ctypes.c_char_p * num)()
		mobiusdll.DllGetAllResults(self.datasetptr, namearray, typearray, '__all!!__')
		check_dll_error()
		return [(name.decode('utf-8'), type.decode('utf-8')) for name, type in zip(namearray, typearray)]
		
	def get_input_list(self) :
		'''
		Get the name and type of all the equations in the model as a list of pairs of strings.
		'''
		num = mobiusdll.DllGetAllInputsCount(self.datasetptr)
		check_dll_error()
		namearray = (ctypes.c_char_p * num)()
		typearray = (ctypes.c_char_p * num)()
		mobiusdll.DllGetAllInputs(self.datasetptr, namearray, typearray)
		check_dll_error()
		return [(name.decode('utf-8'), type.decode('utf-8')) for name, type in zip(namearray, typearray)]
		
	def input_was_provided(self, name, indexes):
		'''
		Find out if a particular input timeseries was provided in the input file (or set maually using set_input_series), or if it is missing.
		
		Arguments:
			name             -- string. The name of the input series. Example : "Air temperature"
			indexes          -- list of strings. A list of index names to identify the particular input series. Example : ["Langtjern"] or ["Langtjern", "Forest"]
			
		Returns:
			a boolean
		'''
		value = mobiusdll.DllInputWasProvided(self.datasetptr, _CStr(name), _PackIndexes(indexes), len(indexes))
		check_dll_error()
		return value
		
	def get_branch_inputs(self, indexsetname, indexname):
		'''
		Get the branch inputs of an index in a branched index set.
		
		Arguments:
			indexsetname     -- string. The name of an index set. This index set has to be branched.
			indexname        -- string. The name of an index in this index set.
			
		Returns:
			a list of strings containing the names of the indexes that are branch inputs to the given index.
		'''
		num = mobiusdll.DllGetBranchInputsCount(self.datasetptr, _CStr(indexsetname), _CStr(indexname))
		check_dll_error()
		namearray = (ctypes.c_char_p * num)()
		mobiusdll.DllGetBranchInputs(self.datasetptr, _CStr(indexsetname), _CStr(indexname), namearray)
		check_dll_error()
		return [name.decode('utf-8') for name in namearray]
		