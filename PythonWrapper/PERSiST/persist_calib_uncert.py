import numpy as np
import importlib.util
import pickle
from scipy.stats import norm
import os

# Initialise wrapper
# Note that the paths assume /home/nras/Mobius/ as the root directory
# Correct path to mobius.py
# Get the current working directory
current_dir = os.getcwd()
wrapper_fpath = os.path.join(current_dir, 'PythonWrapper', 'mobius.py')
spec = importlib.util.spec_from_file_location('mobius', wrapper_fpath)
wr = importlib.util.module_from_spec(spec)
spec.loader.exec_module(wr)
wr.initialize('/home/nras/Mobius/Applications/Persist/persist.so')

# Calibration functions
calib_fpath = os.path.join(current_dir, 'PythonWrapper', 'mobius_calib_uncert_lmfit.py')
spec = importlib.util.spec_from_file_location('mobius_calib_uncert_lmfit', calib_fpath)
cu = importlib.util.module_from_spec(spec)
spec.loader.exec_module(cu)

def log_likelihood(params, error_param_dict, comparisons, skip_timesteps=0):
    """ Log-likelihood assuming heteroscedastic Gaussian errors.
    
    Args:
        params:           Obj. LMFit 'Parameters' object
        error_param_dict: Dict. Maps observed series to error terms e.g.
                              
                              {'Observed Q':'err_q'}
                          
                          Error terms must be named 'err_XXX'                              
        comparisons:      List. Datasets to be compared
        skip_timesteps:   Int. Number of steps to skip before performing comparison
        
    Returns:
        Float. Total log-likelihood.
    """   
    if 'observed Q' not in error_param_dict:
        raise KeyError("The key 'observed Q' is missing from error_param_dict")
    # Update parameters and run model
    dataset_copy = dataset.copy()
    cu.set_parameter_values(params, dataset_copy)
    dataset_copy.run_model()
    
    ll_tot = 0
    
    for i, comparison in enumerate(comparisons):
        simname, simindexes, obsname, obsindexes = comparison
    
        sim = dataset_copy.get_result_series(simname, simindexes)
        obs = dataset_copy.get_input_series(obsname, obsindexes, alignwithresults=True)
        
        sim = sim[skip_timesteps:]
        obs = obs[skip_timesteps:]
        
        error_par = params[error_param_dict[obsname]].value
        sigma_e = error_par*sim
      
        ll = norm(sim, sigma_e).logpdf(obs)
        
        ll_tot += np.nansum(ll)
    
    dataset_copy.delete()
    
    #print(ll_tot)    
    
    return ll_tot

###################################################################################################################

#dataset_fpath = os.path.join(current_dir, 'Applications', 'Persist', 'optimized_params.dat')

dataset = wr.DataSet.setup_from_parameter_and_input_files('/home/nras/Mobius/PythonWrapper/PERSiST/optimized_params.dat', '/home/nras/Mobius/Applications/Persist/Tarland/persist_inputs_Tarland.dat')
#dataset = wr.DataSet.setup_from_parameter_and_input_files('..\..\Applications\Persist\Haelva\optimized_params.dat', '..\..\Applications\Persist\Haelva\persist_inputs_Haelva.dat')

if __name__ == '__main__': # NOTE: this is necessary for parallelisation!
    
    # Unpack options from pickled file
    with open('/home/nras/Mobius/PythonWrapper/PERSiST/pickled/mcmc_settings.pkl', 'rb') as handle:
        settings_dict = pickle.load(handle)

    params = settings_dict['params']
    error_param_dict = settings_dict['error_param_dict']
    comparisons = settings_dict['comparisons']
    skip_timesteps = settings_dict['skip_timesteps']
    nworkers = settings_dict['nworkers']
    ntemps = settings_dict['ntemps']
    nsteps = settings_dict['nsteps']
    nwalk = settings_dict['nwalk']
    nburn = settings_dict['nburn']
    thin = settings_dict['thin']
    init_chains = settings_dict['init_chains']
    result_path = settings_dict['result_path'] 
    chain_path = settings_dict['chain_path']
    corner_path = settings_dict['corner_path']

    # Perform MCMC sampling (but keep everything at present i.e. no burning or thinning)
    result = cu.run_mcmc(log_likelihood, params, error_param_dict, comparisons, nworkers=nworkers,
                         ntemps=ntemps, nsteps=nsteps, nwalk=nwalk, nburn=0, thin=1, start=init_chains,
                         fcn_args=(error_param_dict, comparisons), 
                         fcn_kws={'skip_timesteps':skip_timesteps})

    # Save results
    with open(result_path, 'wb') as output:
        pickle.dump(result, output)
        
    # Plotting
    cu.chain_plot(result, file_name=chain_path)
    # cu.triangle_plot(result, nburn, thin, file_name=corner_path)
    # 
    # MAP simulation
    cu.set_parameter_values(result.params, dataset)
    dataset.run_model()
    cu.plot_objective(dataset, comparisons)
     
    # Goodness-of-fit stats
    cu.gof_stats(result, dataset, comparisons, skip_timesteps, use_stat = None)

