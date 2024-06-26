
from importlib.machinery import SourceFileLoader
import lmfit

# Initialise wrapper
cu = SourceFileLoader("mobius_calib_uncert_lmfit", r"..\mobius_calib_uncert_lmfit.py").load_module()


def configure_params(params, do_doc, do_hydro, num_lu=3, relative_conc=False, has_lake=False, mineral_soil=False):
	
	
	if num_lu == 1 :
		if do_hydro :
			
			params['alphaPET_A'].min = 0.3
			params['alphaPET_A'].max = 2.5
			
			params['fquick'].min = 0.0
			params['fquick'].max = 0.01
			
			params['Ts_A'].min = 1
			params['Ts_A'].max = 15
			
			if mineral_soil :
				params['Tg'].min = 10
				params['Tg'].max = 200
				
				params['bfi'].min = 0.01
				params['bfi'].max = 0.2        # If higher, some of the catchments behave badly

		if do_doc :
			
			params['fc_A'].min = 60
			params['fc_A'].max = 300
		
			params['depthST'].min = -1.5
			params['depthST'].max = -0.01
			
			params['STC_A'].min = 0.05
			params['STC_A'].max = 0.3
			
			params['kT1'].min = 0.0
			params['kT1'].max = 0.2
			
			params['kT2'].min = -0.002
			params['kT2'].max = 0.002

			params['kSO4'].min = 0.0
			params['kSO4'].max = 0.002
			
			params['baseDOC_A'].min = 1
			params['baseDOC_A'].max = 35
			
			params['cDOC'].max = 3
			
			if has_lake :
				params['DOCrad'].min = 0
				params['DOCrad'].max = 0.001
				
			if mineral_soil :
				params['DOChlgw'].min = 30
				params['DOChlgw'].max = 200
			
	elif num_lu == 2 :
		if do_hydro :
			
			params['alphaPET_F'].min = 0.3
			params['alphaPET_F'].max = 2.3
			#TODO: Should this be different per land type or not?
			params['alphaPET_P'].set(expr='alphaPET_F')
			
			params['fquick'].min = 0.0
			params['fquick'].max = 0.02
			
			params['Ts_F'].min = 1
			params['Ts_F'].max = 6
			
			params['Ts_P'].min = 1
			#params['Ts_P'].min = 5
			#params['Ts_P'].max = 20
			params['Ts_P'].max = 7

		if do_doc :
			
			params['fc_F'].min = 40
			params['fc_F'].max = 200
			
			params['fc_P'].min = 100
			params['fc_P'].max = 400
		
			params['depthST'].min = -1.5
			params['depthST'].max = -0.01
			
			params['STC_F'].min = 0.05
			params['STC_F'].max = 0.3
			params['STC_P'].set(expr='STC_F')
			
			params['kT1'].min = 0.0
			params['kT1'].max = 0.2

			params['kSO4'].min = 0.0
			params['kSO4'].max = 0.002
			
			params['baseDOC_F'].min = 1
			#params['baseDOC_F'].max = 25
			params['baseDOC_F'].max = 9
			
			
			#params['baseDOC_P'].min = 4
			if relative_conc :
				params['aux_baseDOC_P'].min = 1.0
				params['aux_baseDOC_P'].max = 5.0
			else :
				params['baseDOC_P'].min = 8
				params['baseDOC_P'].max = 50
			
			
			params['cDOC'].max = 3

	elif num_lu == 3 :
		if do_hydro :
			
			params['alphaPET_F'].min = 0.5
			params['alphaPET_F'].max = 2.3
			#TODO: Should this be different per land type or not?
			for lu in ['S', 'P']:
				params['alphaPET_%s' % lu].set(expr='alphaPET_F')
			
			params['fquick'].min = 0.0
			params['fquick'].max = 0.1
			
			params['Ts_F'].min = 1
			params['Ts_S'].min = 1
			params['Ts_P'].min = 5
			
			params['Ts_F'].max = 6
			params['Ts_S'].max = 6
			params['Ts_P'].max = 20

		if do_doc :
			
			params['fc_F'].min = 60
			params['fc_S'].min = 40
			params['fc_P'].min = 100
		
			params['fc_F'].max = 200
			params['fc_S'].max = 100
			params['fc_P'].max = 400
		
			params['depthST'].min = -1
			params['depthST'].max = -0.1
			
			params['STC_F'].min = 0.05
			params['STC_F'].max = 0.3
			params['STC_S'].set(expr='STC_F')
			params['STC_P'].set(expr='STC_F')
			
			params['kT1'].min = 0.0
			params['kT1'].max = 0.2

			params['kSO4'].min = 0.0
			params['kSO4'].max = 0.002
			
			params['baseDOC_F'].min = 3
			params['baseDOC_S'].min = 1
			params['baseDOC_P'].min = 8
			
			params['baseDOC_F'].max = 9
			params['baseDOC_S'].max = 4
			params['baseDOC_P'].max = 50
			

			params['cDOC'].max = 3
	
	
def setup_calibration_params(dataset, do_doc=True, do_hydro=True, relative_conc=False) :
	
	num_lu = len(dataset.get_indexes('Landscape units'))
	haslake = (len(dataset.get_indexes('Reaches')) > 1)
	
	#dataset.set_parameter_enum('Deep soil/groundwater DOC computation', [], 'bliff')
	test = dataset.get_parameter_enum('Deep soil/groundwater DOC computation', [])
	#print(test)
	
	mineral_soil = (test == 'mass_balance')
	
	if num_lu == 1 :
		param_df = cu.get_double_parameters_as_dataframe(dataset, index_short_name={'All':'A', 'River':'r0'})
	elif num_lu == 2:
		param_df = cu.get_double_parameters_as_dataframe(dataset, index_short_name={'LowC':'F', 'HighC':'P', 'River':'r0'})
	elif num_lu == 3 :
		param_df = cu.get_double_parameters_as_dataframe(dataset, index_short_name={'Forest':'F', 'Shrubs':'S', 'Peat':'P', 'River':'r0'})
	
	wantparams = []
	
	wantparams1 = [
			'alphaPET', 'DDFmelt', 
			'fquick', 'Ts',
	]
	wantparams2 = [
			'fc', 'depthST', 'STC',
			'kT', 'kSO4', 'cDOC',
	]
	if relative_conc :
		wantparams2 += ['baseDOC_F']
	else :
		wantparams2 += ['baseDOC']
		
	if haslake :
		wantparams2 += ['DOCrad']
	
	if mineral_soil :
		wantparams1 += ['bfi', 'Tg']
		wantparams2 += ['DOChlgw']
	
	if do_hydro :
		wantparams += wantparams1
	if do_doc :
		wantparams += wantparams2

	#Select all parameters with short_name starting with any of the names in 'wantparams'
	calib_df = param_df[[any([sn.startswith(n) for n in wantparams]) for sn in param_df['short_name']]]
	
	params = cu.parameter_df_to_lmfit(calib_df)
	if relative_conc :
		params.add(lmfit.Parameter('aux_baseDOC_P', value=1.0, min=1.0, max=5.0, user_data={'parameter_type':'model_parameter'}))
	
	#print('Params before config')
	#params.pretty_print()
	
	configure_params(params, do_doc, do_hydro, num_lu, relative_conc, haslake, mineral_soil)
	
	return params