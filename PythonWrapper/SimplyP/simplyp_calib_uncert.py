import numpy as np
import types
import importlib
import importlib.util
import importlib.machinery
#import imp
import pickle
from scipy.stats import norm

# Initialise wrapper
wrapper_fpath = (r'../mobius.py')
#wr = imp.load_source('mobius', wrapper_fpath)
#wr.initialize('../../Applications/SimplyP/simplyp.so')
#wr.initialize('../../Applications/SimplyP/simplyp.dll')
# Load the module from the specified file path
spec = importlib.util.spec_from_file_location('mobius', wrapper_fpath)
mobius_module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mobius_module)

# Initialize the module
mobius_module.initialize('../../Applications/SimplyP/simplyp.so')

# Calibration functions
calib_fpath = (r'../mobius_calib_uncert_lmfit.py')
#cu = imp.load_source('mobius_calib_uncert_lmfit', calib_fpath)
speccu = importlib.util.spec_from_file_location('mobius_calib_uncert_lmfit', calib_fpath)
mobius_modulecu = importlib.util.module_from_spec(speccu)
speccu.loader.exec_module(mobius_modulecu)



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

dataset = wr.DataSet.setup_from_parameter_and_input_files('../../Applications/SimplyP/Tarland/TarlandParameters_v0-3.dat', 
                                                          '../../Applications/SimplyP/Tarland/TarlandInputs.dat')

if __name__ == '__main__': # NOTE: this is necessary for parallelisation!
    
    # Unpack options from pickled file
    with open('results/mcmc_settings.pkl', 'rb') as handle:
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
    
    # Update median and MAP estimates to reflect nburn and thin
    result = cu.update_mcmc_results(result, nburn, thin)

    # Save results
    with open(result_path, 'wb') as output:
        pickle.dump(result, output)
        
    # Plotting
    cu.chain_plot(result, file_name=chain_path)
    cu.triangle_plot(result, nburn, thin, file_name=corner_path)

    # MAP simulation
    cu.set_parameter_values(result.params, dataset, use_stat='map')
    dataset.run_model()
    cu.plot_objective(dataset, comparisons)

    # Goodness-of-fit stats
    cu.gof_stats(result, dataset, comparisons, skip_timesteps, use_stat='map')
