
install.packages('tidyverse')
install.packages("shiny")
install.packages("xgboost")
install.packages("data.table")
install.packages("pdp")
install.packages("iml")
install.packages("ParBayesianOptimization")
install.packages("Hmisc")
install.packages("Ckmeans.1d.dp")
install.packages("htmlwidgets")
install.packages("shinythemes")
install.packages("shinyWidgets")
install.packages("DT")
install.packages("shinyalert")

# APP FOR GBM FITTING
require(shiny)
require(tidyverse)
require(xgboost)
require(pdp)
require(ParBayesianOptimization)
require(data.table)
require(iml)
require(Hmisc)
require(Ckmeans.1d.dp)
require(plotly)
require(htmlwidgets)
require(shinythemes)
require(shinyWidgets)
require(DT)
require(shinyjs)
require(shinyalert)

options(dplyr.summarise.inform = FALSE)
options(shiny.maxRequestSize=30*1024^2)







#---------------------------------------------------------------------------------------#
# ~~ Required functions----
#---------------------------------------------------------------------------------------#

partition_data_seeded <- function( in_data, in_partition_seed, in_sample_frac, in_cut_points ){
  # set seed
  set.seed( in_partition_seed )
  # sampled data
  sampled_data <- in_data %>% sample_frac( in_sample_frac )
  # generate random index
  random_index_for_partition <- runif( nrow( sampled_data ) )
  
  train_index <- which( random_index_for_partition <= in_cut_points[[1]] )
  test_index  <- which( random_index_for_partition > in_cut_points[[1]] & random_index_for_partition <= in_cut_points[[2]]  )
  valid_index <- which( random_index_for_partition > in_cut_points[[2]] )
  # split data
  out_partitioned_data <- list(sampled_data[train_index,], sampled_data[test_index,], sampled_data[valid_index,])
  return(out_partitioned_data)
}

partition_data_factor <- function( in_data, in_partition_factor ){
  partition_symbol <- sym(in_partition_factor)
  out_partitioned_data <- list(filter(in_data, !!partition_symbol == 1),
                               filter(in_data, !!partition_symbol == 2),
                               filter(in_data, !!partition_symbol == 3))
  return(out_partitioned_data)
}


char_to_factor <- function(d_in){
  d_char <- d_in %>% select_if(~is_character(.) & length(unique(.))>1)
  d_num <- d_in %>% select_if(~!(is_character(.)))
  if ( length(d_char) > 0 ){
    d_char_na <- d_char %>% mutate( across(everything(), ~replace_na(.x, "NA"))) %>% mutate( across(everything(), ~make.names(.x)))
    d_out <- d_char_na %>%
      map_dfc(as.factor) %>% 
      cbind(d_num)
  } else {
    d_out <- d_num
  }
  return(d_out)
  
}

clean_char_data <- function(d_in){
  d_char <- d_in %>% select_if(~is.factor(.) & length(unique(.))>1)
  d_num <- d_in %>% select_if(~!(is.factor(.)))
  if ( length(d_char) > 0 ){
    d_out <- d_char %>%
      Matrix::sparse.model.matrix(object = ~.-1.,
                                  data = .,
                                  drop.unused.levels = FALSE,
                                  contrasts.arg = map(., contrasts, contrasts = FALSE)
      ) %>%
      as.matrix() %>% as.data.frame() %>%
      cbind(d_num)
    
  } else {
    d_out <- d_num
  }
  return(d_out)
}





band_data <- function( in_data, in_factor_information, in_noband ){
  out_data <- in_data %>%
    mutate_at(filter(in_factor_information, needs_banding==1)$base_feature,
              ~Hmisc::cut2(.,g=in_noband)) %>% 
    mutate(xgb_prediction = 0)
  return(out_data)
}



transpose_and_summarise <- function( in_data, response_symbol, exposure_symbol ){
  out_data <- in_data %>%
    map_df(as.character) %>%
    pivot_longer(cols = -c(!!response_symbol, !!exposure_symbol, xgb_prediction)) %>% 
    group_by(name, value) %>% 
    summarise(reponse_sum = sum(as.numeric(!!response_symbol)),
              response_prediction = sum(as.numeric(xgb_prediction)),
              exposure_sum = sum(as.numeric(!!exposure_symbol))) %>% 
    mutate(actual = reponse_sum / exposure_sum,
           prediction = response_prediction / exposure_sum) %>% 
    arrange(name, value)
}



# function to select the longest partial match in a list
str_which_max_length <- function( in_match, in_lookup ){
  str_list <- in_lookup[str_which(in_match, paste0("^",in_lookup))]
  longest_str <- str_list[nchar(str_list) == max(nchar(str_list))][1]
  return(longest_str)
}


collate_factor_information <- function( in_subset_data_train, in_factor_list, in_factor_list_ohe ){
  out_data <- in_factor_list_ohe %>% as.data.frame()
  colnames(out_data) <- "ohe_feature"
  # map ohe factors to base factors
  # TO DO - may be nicer as a mutate rather than a map_chr ----
  out_data$base_feature <- map_chr(out_data$ohe_feature, str_which_max_length, in_factor_list)
  out_data <- out_data %>% rowwise %>% 
    mutate( feature_type = typeof(in_subset_data_train[[base_feature]]),
            numerical = is.numeric(in_subset_data_train[[base_feature]]),
            levels = n_distinct(in_subset_data_train[[base_feature]]),
            needs_banding = 0)
  return(out_data)
}


get_model_type <- function(model_type){
  objective_function <- switch(model_type, 'count:poisson', 'reg:gamma', 'reg:tweedie', 'binary:logistic')
  eval_for_fit       <- switch(model_type, 'poisson-nloglik', 'gamma-deviance', 'tweedie-nloglik@1.5', 'logloss')
  eval_for_tune      <- switch(model_type, 'test_poisson_nloglik_mean', 'test_gamma_deviance_mean', 'test_tweedie_nloglik@1.5_mean', 'test_logloss')
  model_type_str     <- switch(model_type, 'Frequency', 'Severity', 'Burning Cost', 'Binomial')
  
  return(list(objective_function,eval_for_fit,eval_for_tune,model_type_str))
}

# parameter tuning ---------------------------------------------------------------------------------------------------------


# defining our base scoring function
base_scoring_function <- function(in_data,
                                  in_model_specs,
                                  in_factor_list,
                                  in_response,
                                  in_exposure,
                                  in_mon_constraints,
                                  in_offset,
                                  in_time,
                                  tuned_eta,
                                  tuned_min_child_weight, 
                                  tuned_max_delta_step, 
                                  tuned_max_depth, 
                                  tuned_subsample, 
                                  tuned_gamma, 
                                  tuned_alpha, 
                                  tuned_lambda,
                                  tuned_early_stopping){
  
  full_parameter_list <- list(booster = "gbtree",
                              eta = tuned_eta,
                              max_delta_step = tuned_max_delta_step,
                              min_child_weight = tuned_min_child_weight,
                              max_depth = tuned_max_depth,
                              subsample = tuned_subsample,
                              gamma = tuned_gamma,
                              alpha = tuned_alpha,
                              lambda = tuned_lambda,
                              monotone_constraints = in_mon_constraints)
  
  tuning_data <- in_data %>% list %>% prep_xgboost_data(in_factor_list, in_response, in_exposure, in_offset, in_time)
  
  xgbcv <<- xgb.cv(params = full_parameter_list,
                   data = tuning_data[[1]],
                   nround = 2000,
                   nfold = 5,
                   objective = in_model_specs[[1]],
                   eval_metric = in_model_specs[[2]],
                   prediction = TRUE,
                   showsd = TRUE,
                   early_stopping_rounds = tuned_early_stopping,
                   maximise = FALSE,
                   verbose = TRUE)
  
  return(list(Score = max(-xgbcv$evaluation_log[[in_model_specs[[3]]]]), nrounds = xgbcv$best_iteration ) )
}

# defining our scoring function depending on which variables are to be tuned
create_partial_scoring_function <- function(in_data,in_model_specs,in_factor_list,in_response,in_exposure,in_fixed_parameters,in_mon_constraints,in_offset, in_time){
  fixed_list <- c(list(.f=base_scoring_function,in_data=in_data, in_model_specs=in_model_specs,
                       in_factor_list=in_factor_list,in_response=in_response,in_exposure=in_exposure,
                       in_mon_constraints = in_mon_constraints, in_offset=in_offset, in_time=in_time), in_fixed_parameters)
  partial_scoring_function <- do.call(purrr::partial, fixed_list)
  return(partial_scoring_function)
} 

# wrapper function for parameter tuning
parameter_tuning <- function(in_data, in_model_specs, in_parameter_df,in_factor_list,in_response,in_exposure, in_mon_constraints, in_offset, in_time)
{
  # define bounds
  variable_params_int <- in_parameter_df %>% filter(integer_var == 1, tune_flag == 1)
  tuning_bounds_int <- map2( .x=as.integer(variable_params_int$min), .y=as.integer(variable_params_int$max), .f=c )
  names(tuning_bounds_int) <- variable_params_int$tuning_parameter  
  
  variable_params_num <- in_parameter_df %>% filter(integer_var == 0, tune_flag == 1)
  tuning_bounds_num <- map2( .x=variable_params_num$min, .y=variable_params_num$max, .f=c )
  names(tuning_bounds_num) <- variable_params_num$tuning_parameter  
  
  tuning_bounds <- c(tuning_bounds_int,tuning_bounds_num)
  
  # define the values of fixed parameters
  fixed_parameters <- in_parameter_df %>% filter(tune_flag == 0) %>% select(tuning_parameter, value) %>% deframe
  
  # pass fixed parameter list to create scoring function
  score_fun <- create_partial_scoring_function(in_data, in_model_specs,in_factor_list,in_response,in_exposure, fixed_parameters, in_mon_constraints, in_offset, in_time)
  
  #tune parameters
  bayes_params <- bayesOpt(FUN = score_fun, bounds = tuning_bounds, initPoints = 12, iters.n = 20, acq = "ei", verbose = 2)
  
  return(enframe(getBestPars(bayes_params), name = "tuning_parameter", value = "tuned_value"))
}

# model development --------------------------------------------------------------------------------------------------------

create_dmatrix <- function( in_clean_data, in_base_margin, in_factor_list, in_response ){
  #create_dmatrix <- function( in_clean_data, in_base_margin, in_factor_list, in_response, in_exposure ){
  out_dmatrix <- xgb.DMatrix(data  = in_clean_data[,in_factor_list, drop = FALSE], label = in_clean_data[,in_response])
  setinfo(out_dmatrix,"base_margin",in_base_margin)
  #setinfo(out_dmatrix,"weight",in_clean_data[,in_exposure])
  return(out_dmatrix)
}

prep_xgboost_data <- function( in_clean_data, in_factor_list, in_response, in_exposure, in_offset = NULL, in_time ){
  in_response_sym <- sym(in_response)
  in_exposure_sym <- sym(in_exposure)
  if(is.null(in_offset)){
    if(is.null(in_time)){
      means_list   <- in_clean_data %>% map( ~ sum(.[,in_response])/sum(.[,in_exposure]))
    } else {
      means_list   <- in_clean_data %>% 
        map(as.data.frame) %>% 
        map(group_by, across(starts_with(in_time))) %>%
        map(transmute, result = sum(!!in_response_sym)/sum(!!in_exposure_sym)) %>% 
        map(pull,result)
    }
    base_margins <- in_clean_data %>% map2( means_list, ~ log(.x[,in_exposure]) + log(.y) )
    dmatrix_list <- in_clean_data %>% map2( base_margins, create_dmatrix, in_factor_list, in_response )
    #dmatrix_list <- in_clean_data %>% map2( base_margins, create_dmatrix, in_factor_list, in_response, in_exposure )
  } else {
    base_margins <- in_clean_data %>% map( ~ log(.[,in_exposure]) + log(.[,in_offset]) )
    dmatrix_list <- in_clean_data %>% map2( base_margins, create_dmatrix, in_factor_list, in_response )
    #dmatrix_list <- in_clean_data %>% map2( base_margins, create_dmatrix, in_factor_list, in_response, in_exposure )
  }
  return(dmatrix_list)
}

fit_xgboost_model <- function( in_prepared_data, in_parameter_list, in_early_stopping, nbround,in_model_specs ){
  # set watchlist
  watchlist_xy <- list(train = in_prepared_data[[1]], test = in_prepared_data[[2]])  
  # fit model
  xgb_fit_model <- xgb.train(params = in_parameter_list,
                             data = in_prepared_data[[1]], 
                             nround = nbround,
                             watchlist = watchlist_xy, 
                             print_every_n = 100,
                             early_stopping_rounds = in_early_stopping,
                             objective = in_model_specs[[1]], 
                             eval_metric = in_model_specs[[2]],
                             maximize = FALSE,
                             verbose = TRUE,
                             base_score = 1,
                             disable_default_eval_metric = TRUE
  )
  return(xgb_fit_model)
}

create_gains_data <- function( in_prediction_symbol, in_banded_data_w_pred, in_exposure_symbol, in_response_symbol ){
  out_gains_data <- in_banded_data_w_pred %>%
    map( arrange, desc( !!in_prediction_symbol ) ) %>%
    map( mutate,
         cumulative_exposure = cumsum( !!in_exposure_symbol ) / sum( !!in_exposure_symbol ),
         cumulative_response = cumsum( !!in_response_symbol ) / sum( !!in_response_symbol ))
  return( out_gains_data )
}

return_xgboost_predictions <- function( in_banded_data, in_prepared_data, in_xgb_model, in_response_symbol, in_exposure_symbol ){
  
  in_banded_data[[1]]$xgb_prediction <- predict(in_xgb_model, in_prepared_data[[1]])
  in_banded_data[[2]]$xgb_prediction <- predict(in_xgb_model, in_prepared_data[[2]])
  in_banded_data[[3]]$xgb_prediction <- predict(in_xgb_model, in_prepared_data[[3]])
  
  in_banded_data[[1]]$margin_prediction <- predict(in_xgb_model, in_prepared_data[[1]], outputmargin = TRUE) - getinfo(in_prepared_data[[1]], 'base_margin')
  in_banded_data[[2]]$margin_prediction <- predict(in_xgb_model, in_prepared_data[[2]], outputmargin = TRUE) - getinfo(in_prepared_data[[2]], 'base_margin')
  in_banded_data[[3]]$margin_prediction <- predict(in_xgb_model, in_prepared_data[[3]], outputmargin = TRUE) - getinfo(in_prepared_data[[3]], 'base_margin')
  
  out_long_data <- in_banded_data %>% map( transpose_and_summarise, in_response_symbol, in_exposure_symbol )
  
  out_gains_data <- list( sym("margin_prediction"), in_response_symbol) %>% 
    map( create_gains_data, in_banded_data, in_exposure_symbol, in_response_symbol )
  
  return(list(in_banded_data, out_long_data, out_gains_data[[1]], out_gains_data[[2]]))
}

append_base_features <- function( in_imp_table_ohe, in_factor_information ){
  out_imp_table_base <- in_imp_table_ohe %>%
    left_join( in_factor_information, by = c( 'Feature' = 'ohe_feature' ) ) %>%
    select( base_feature, Gain, Cover, Frequency ) %>%
    rename( Feature = base_feature ) %>% 
    group_by( Feature ) %>% 
    summarise( Gain = sum(Gain),
               Cover = sum(Cover),
               Frequency = sum(Frequency) ) %>% 
    setDT()
  return(out_imp_table_base)
}



# model results ------------------------------------------------------------------------------------------------------------

trap_integrate_gini <- function( in_data ){
  out_data <- in_data %>% 
    approx( y = .$cumulative_response, 
            x = .$cumulative_exposure,
            xout = (0:100)*0.01, method = "linear" )
  result <- (sum(out_data$y, na.rm = T) - 0.5) / 100
  return(result * 2 - 1)  
}

calculate_gini_score <- function( in_data ){ return( map_dbl( in_data, trap_integrate_gini ) ) }

# plotting functions ----

plot_gains <- function( in_partition, in_gains, in_gains_opt ){
  out_gains_plot <- ggplot( ) + 
    geom_line( data = in_gains[[as.numeric(in_partition)]],
               mapping = aes( x = cumulative_exposure, y = cumulative_response ), col = "#FFCB05", size = 1 ) +
    geom_line( data = in_gains_opt[[as.numeric(in_partition)]],
               mapping = aes( x = cumulative_exposure, y = cumulative_response ) ) +
    geom_abline(col = "grey") + theme_bw()
  return(out_gains_plot)
}


# observation explorer -----------------------------------------------------------------------------------------------------




lift_func <- function( data, exposure, response_var, prediction, no_bands, log=F, scale=F ) {
  
  pred <- sym( prediction )
  act  <- sym( response_var )
  if(exposure == ''){
    exp  <- sym( 'Exposure' )
    d <- data %>% 
      arrange( !!act ) %>% 
      mutate( Exposure = 1,
              cumexposure = cumsum(!!exp) )
  } else{
    exp  <- sym( exposure )
    d <- data %>% 
      arrange( !!act ) %>% 
      mutate( cumexposure = cumsum(!!exp) )
  }
  
  
  lb <- min(d$cumexposure)
  ub <- max(d$cumexposure)
  w <- ceiling(( ub - lb ) / no_bands)
  
  cut_vector <- seq( lb,
                     ub,
                     w )
  
  result_vector <- as.integer( cut2( d$cumexposure,
                                     cut_vector ))
  
  banded_df <- data.frame(cbind(d,result_vector)) 
  
  if(log==F){
    if(scale==F){
      lift_df <- banded_df %>% 
        group_by( result_vector ) %>% 
        summarise( prediction = sum( !!pred * !!exp ),
                   actual   = sum( !!act ),
                   exposure = sum( !!exp ) )
    }
    else{
      lift_df <- banded_df %>% 
        group_by( result_vector ) %>% 
        summarise( prediction = sum( !!pred * !!exp ),
                   actual   = sum( !!act ),
                   exposure = sum( !!exp ) ) %>% 
        ungroup() %>%
        mutate( prediction = prediction/mean(prediction)*mean(actual),
                actual = actual)
    }  
  }
  else{
    if(scale==F){
      lift_df <- banded_df %>% 
        group_by( result_vector ) %>% 
        summarise( prediction = log(sum( !!pred * !!exp )),
                   actual   = log(sum( !!act )),
                   exposure = sum( !!exp ) )
    }
    else{
      lift_df <- banded_df %>% 
        group_by( result_vector ) %>% 
        summarise( prediction = sum( !!pred * !!exp ),
                   actual   = sum( !!act ),
                   exposure = sum( !!exp ) ) %>% 
        ungroup() %>%
        mutate( prediction = log(prediction/mean(prediction)*mean(actual)),
                actual = log(actual))
    }
  }
  
  return( lift_df )
  
}


plot_lifts <- function( in_partition, in_lift){
  out_lift_plot <-  ggplot( data = in_lift[[as.numeric(in_partition)]] ) +
    geom_line( mapping = aes( x = result_vector, y = prediction, colour = "Modelled"  ), size = 1 ) +
    geom_line( mapping = aes( x = result_vector, y = actual    , colour = "Actual" ), size = 1 ) +
    scale_colour_manual(values = c( 'Modelled' = '#FFCB05', 'Actual' = 'grey' )) +
    labs(colour = 'Response') + 
    theme_bw() + ggtitle('Lift Curve')
  
  return(out_lift_plot)
}




return_xgboost_predictions <- function( in_banded_data, in_prepared_data, in_xgb_model, in_response_symbol, in_exposure_symbol ){
  
  in_banded_data[[1]]$xgb_prediction <- predict(in_xgb_model, in_prepared_data[[1]])
  in_banded_data[[2]]$xgb_prediction <- predict(in_xgb_model, in_prepared_data[[2]])
  in_banded_data[[3]]$xgb_prediction <- predict(in_xgb_model, in_prepared_data[[3]])
  
  in_banded_data[[1]]$margin_prediction <- predict(in_xgb_model, in_prepared_data[[1]], outputmargin = TRUE) - getinfo(in_prepared_data[[1]], 'base_margin')
  in_banded_data[[2]]$margin_prediction <- predict(in_xgb_model, in_prepared_data[[2]], outputmargin = TRUE) - getinfo(in_prepared_data[[2]], 'base_margin')
  in_banded_data[[3]]$margin_prediction <- predict(in_xgb_model, in_prepared_data[[3]], outputmargin = TRUE) - getinfo(in_prepared_data[[3]], 'base_margin')
  
  
  
  out_gains_data <- list( sym("margin_prediction"), in_response_symbol) %>% 
    map( create_gains_data, in_banded_data, in_exposure_symbol, in_response_symbol )
  
  return(list(in_banded_data,  out_gains_data[[1]], out_gains_data[[2]]))
}




summarise_fun<-function (in_data, in_fac_list,in_exposure,in_response) {
  result<-list()
  in_exp <- sym(in_exposure)
  in_res <- sym(in_response)
  
  for(fac in in_fac_list) {
    fac_sym <- sym(fac)
    
    result[[fac]]<-in_data %>% group_by(!!fac_sym) %>%
      summarise(exp_sum = sum(!!in_exp),
                res_sum = sum(!!in_res))  %>%
      mutate(actual=res_sum/exp_sum,
             prediction = 0,
             AW_shap_premium = 0)
    setnames(result[[fac]], old = c(fac,'exp_sum'), new = c('factor_name','exposure'))
    
  }
  return(result)
}



summarise_with_pred_fun<-function (in_data, in_fac_list,in_exposure,in_response,pred_name) {
  result<-list()
  in_exp <- sym(in_exposure)
  in_res <- sym(in_response)
  pred <- sym(pred_name)
  average_prem <- sum(in_data[[pred_name]])/sum(in_data[[in_exposure]])
  
  for(fac in in_fac_list) {
    fac_sym <- sym(fac)
    
    result[[fac]]<-in_data %>% group_by(!!fac_sym) %>%
      summarise(w_pred_sum = sum(!!pred * !!in_exp),
                exp_sum = sum(!!in_exp),
                res_sum = sum(!!in_res),
                pred_sum = sum(!!pred))  %>%
      mutate(actual=res_sum/exp_sum,
             prediction = pred_sum/exp_sum,
             AW_shap=1,
             AW_prediction=average_prem,
             AW_shap_premium = AW_prediction)
    setnames(result[[fac]], old = c(fac,'exp_sum'), new = c('factor_name','exposure'))
    
  }
  return(result)
}



summarise_fun_actual<-function (in_data, in_fac_list,in_exposure,in_response) {
  result<-list()
  in_exp <- sym(in_exposure)
  in_res <- sym(in_response)
  
  for(fac in in_fac_list) {
    fac_sym <- sym(fac)
    
    result[[fac]]<-in_data %>% group_by(!!fac_sym) %>%
      summarise(exp_sum = sum(!!in_exp),
                res_sum = sum(!!in_res))  %>%
      mutate(actual=res_sum/exp_sum)
    setnames(result[[fac]], old = c(fac,'exp_sum'), new = c('factor_name','exposure'))
    
  }
  return(result)
}


Actual_Plt <- function(in_data,in_exposure,fac){
  in_data <- in_data[[fac]]
  in_data[['factor_name']] <-  factor(in_data[['factor_name']],exclude=NULL)
  levels(in_data[['factor_name']])[is.na(levels(in_data[['factor_name']]))] <- "NA" 
  
  in_data %>% 
    plot_ly(x = ~factor_name) %>% 
    
    add_trace(y = ~exposure,
              name = "Exposure",
              type = 'bar',
              marker = list(color = 'yellow',
                            line = list(color = 'black',
                                        width = 1.2
                            ))
              #color = I('yellow2')
    ) %>% 
    add_trace(y = ~actual,
              name = "Actual",
              yaxis = "y2",
              mode = 'lines+markers',
              type = 'scatter',
              color = I('magenta3')) %>%
    
    layout(legend = list(x = 1.1, y = 0.5),
           xaxis = list(title = fac), 
           yaxis = list(title = 'Exposure'),
           yaxis2 = list(title = 'Premium',
                         overlaying = "y",
                         side = "right"),
           barmode = "stack")
}  


AvsE_Plt <- function(in_data,in_partition,in_exposure,fac){
  in_data <- in_data[[as.numeric(in_partition)]][[fac]]
  in_data[['factor_name']] <-  factor(in_data[['factor_name']],exclude=NULL)
  levels(in_data[['factor_name']])[is.na(levels(in_data[['factor_name']]))] <- "NA" 
  
  in_data %>% 
    plot_ly(x = ~factor_name) %>% 
    
    add_trace(y = ~exposure,
              name = "Exposure",
              type = 'bar',
              marker = list(color = 'yellow',
                            line = list(color = 'black',
                                        width = 1.2
                            ))
              #color = I('yellow2')
    ) %>% 
    add_trace(y = ~actual,
              name = "Actual",
              yaxis = "y2",
              mode = 'lines+markers',
              type = 'scatter',
              color = I('magenta3')) %>% 
    add_trace(y = ~prediction,
              name = "Expected",
              yaxis = "y2",
              mode = 'lines+markers',
              type = 'scatter',              
              color = I('darkgreen')) %>%
    add_trace(y = ~AW_shap_premium,
              name = "Shap_Model_Approximate",
              yaxis = "y2",
              mode = 'lines+markers',
              type = 'scatter',
              color = I('green')) %>%
    
    layout(legend = list(x = 1.1, y = 0.5),
           xaxis = list(title = fac), 
           yaxis = list(title = 'Exposure'),
           yaxis2 = list(title = 'Premium',
                         overlaying = "y",
                         side = "right"),
           barmode = "stack")
}  


AvsE_with_ref_Plt <- function(in_data,in_partition,in_exposure,fac){
  in_data <- in_data[[as.numeric(in_partition)]][[fac]]
  in_data[['factor_name']] <-  factor(in_data[['factor_name']],exclude=NULL)
  levels(in_data[['factor_name']])[is.na(levels(in_data[['factor_name']]))] <- "NA" 
  
  in_data %>% 
    plot_ly(x = ~factor_name) %>% 
    
    add_trace(y = ~exposure,
              name = "Exposure",
              type = 'bar',
              marker = list(color = 'yellow',
                            line = list(color = 'black',
                                        width = 1.2
                            ))
              #color = I('yellow2')
    ) %>% 
    add_trace(y = ~actual,
              name = "Actual",
              yaxis = "y2",
              mode = 'lines+markers',
              type = 'scatter',
              color = I('magenta3')) %>% 
    add_trace(y = ~prediction,
              name = "Expected",
              yaxis = "y2",
              mode = 'lines+markers',
              type = 'scatter',              
              color = I('darkgreen')) %>%
    add_trace(y = ~prediction_ref,
              name = "Expected_reference_model",
              yaxis = "y2",
              mode = 'lines+markers',
              type = 'scatter',              
              color = I('darkblue')) %>%
    add_trace(y = ~AW_shap_premium,
              name = "Shap_Model_Approximate",
              yaxis = "y2",
              mode = 'lines+markers',
              type = 'scatter',
              color = I('green')) %>%
    add_trace(y = ~AW_shap_premium_ref,
              name = "Shap_Model_reference",
              yaxis = "y2",
              mode = 'lines+markers',
              type = 'scatter',
              color = I('blue')) %>%
    
    layout(legend = list(x = 1.1, y = 0.5),
           xaxis = list(title = fac), 
           yaxis = list(title = 'Exposure'),
           yaxis2 = list(title = 'Premium',
                         overlaying = "y",
                         side = "right"),
           barmode = "stack")
} 



# UI -----------------------------------------------------------------------------------------------------------------------
ui <- navbarPage("XGBoost Modelling Tool", position = "fixed-top",theme = shinytheme("sandstone"),
                 # ~ MAIN TAB 1 - data processing prior modelling -------------------------------------------------------------------------------------
                 tabPanel("Data", tags$style(type="text/css", "body {padding-top: 70px;}"),
                          tabsetPanel(type = "tabs",
                                      # ~ ~ SUB-TAB 1a - data import -------------------------------------------------------------------------------------
                                      tabPanel("Data Import",
                                               br(),
                                               fileInput("action_read_file", "Import data file (Rds.format)",
                                                         width = '50%'),
                                               htmlOutput("output_import_warning"),
                                               br(),
                                               tableOutput("file_info"),
                                               textOutput("number_rows"),
                                               textOutput("number_cols"),
                                               dataTableOutput('table_raw_data_head'),
                                               tableOutput("data_summary_desc")
                                      ),
                                      
                                      # ~ ~ SUB-TAB 1b - data prep ---------------------------------------------------------------------------------------
                                      tabPanel("Data Spec",
                                               br(),
                                               fileInput("action_import_prep", "Choose existing model structure (rds format)",
                                                         width = '30%'),
                                               downloadButton("action_save_prep","Save current model structure",style = "width:30%;"),
                                               br(),
                                               br(),
                                               
                                               fluidRow(
                                                 column(4,
                                                        selectInput("select_model_type",
                                                                    "Model Type",
                                                                    choices = list("Frequency" = 1,
                                                                                   "Severity" = 2,
                                                                                   "Burning Cost" = 3,
                                                                                   "Binomial" = 4)),
                                                        selectInput("select_exposure", "Exposure", choices = list()),
                                                        selectInput("select_response", "Response", choices = list()),
                                                        selectInput("select_offset",   "Offset", choices = list()),
                                                        selectInput("select_time",     "Time factor", choices = list()),
                                                        selectInput("select_data_partition", "Partition type", choices = list( "Factor" = 2)),
                                                        selectInput("select_partition_factor", "Select Partition Factor (require 3 levels)", choices = list()),
                                                        
                                                 ),
                                                 column(4,
                                                        selectInput("select_rating_factors", 
                                                                    "Select Rating factors",
                                                                    choices = list(),
                                                                    multiple=TRUE, 
                                                                    selectize = FALSE,
                                                                    size = 30,
                                                                    width = '70%'),    
                                                        
                                                 ),
                                                 column(4,
                                                        selectInput("select_policy_keys", 
                                                                    "Policy keys",
                                                                    choices = list(),
                                                                    multiple=TRUE, 
                                                                    selectize = FALSE,
                                                                    size = 5),  
                                                        br(),
                                                        
                                                        sliderInput("slider_sample_fraction", "Sample fraction", min = 0.01, max = 1, value = 1, step = 0.01),
                                                        actionButton("action_prepare_data", "Apply model structure",class='btn-primary',width = '50%'),
                                                        htmlOutput("output_data_prep_warning"),
                                                 ),
                                               ),
                                               
                                      ),
                                      tabPanel("Data manipulation",
                                               br(),
                                               fluidRow(
                                                 column(3,
                                                        selectInput("FactorSelect_oneway", "Select factor", choices = list())
                                                 ),
                                                 
                                                 
                                                 column(3, 
                                                        br(),
                                                        radioButtons("mapping_method", "Choose mapping method", choices = list("Manual" = 1,"Auto" = 2), inline = TRUE),
                                                 ),
                                                 
                                                 column(3, 
                                                        br(),
                                                        numericInput("select_bandings",
                                                                     "No. bandings required",
                                                                     value = 20),
                                                 ),
                                                 
                                                 column(3,
                                                        br(),
                                                        br(),
                                                        actionButton("action_modify_factor", "View/create factor mapping",class='btn-primary',width = '80%'),
                                                        br(),
                                                        br(),
                                                        actionButton("action_clear_modification", "Clear all factor mapping",
                                                                     class='btn-primary',width = '80%'),
                                                        br(),
                                                        br(),
                                                        actionButton("action_clear_current_modification", 
                                                                     "Clear current factor mapping",class='btn-primary',width = '80%'),
                                                 ),
                                                 
                                                 
                                               ),
                                               
                                               br(),
                                               fluidRow(
                                                 column(4,
                                                        tags$h4("Original Banding"),
                                                        plotlyOutput("Actual_plot_orig", height = 500),
                                                        
                                                 ),
                                                 column(4,
                                                        tags$h4("Modified Banding"),
                                                        plotlyOutput("Actual_plot", height = 500),
                                                        
                                                 ),
                                                 useShinyjs(),
                                                 column(4,
                                                        actionButton("Pastemapping", 
                                                                     "Paste mapping from Clipboard"),
                                                        br(),
                                                        br(),
                                                        DTOutput("factor_mapping_table"),
                                                        br(),
                                                        fluidRow(
                                                          column(4,
                                                                 actionButton("Modify", 
                                                                              "Confirm the factor mapping",class='btn-primary'),
                                                                 
                                                          ),
                                                          # column(4,
                                                          #        actionButton("action_clear_current_modification", 
                                                          #                     "Clear current factor mapping",class='btn-primary'),
                                                          #        
                                                          # ),
                                                          
                                                        ),
                                                        br(),
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                 )
                                               ),
                                               
                                               
                                               br(),
                                               
                                               actionButton("Prepare_modelling_data", 
                                                            "Create modelling dataset",class='btn-primary',width = '45%'),
                                               htmlOutput("output_modelling_data_warning"),
                                               br(),
                                               br(),
                                               # tags$h3('Automatic numeric banding'),
                                               # fluidRow(
                                               #   
                                               #   column(3,
                                               #          br(),
                                               #          numericInput("select_levels", 
                                               #                       "Benchmark levels", 
                                               #                       value = 100)  
                                               #   ),
                                               #   column(3,
                                               #          br(),
                                               #          numericInput("select_bandings", 
                                               #                       "No. bandings required", 
                                               #                       value = 20) 
                                               #   ),
                                               #   
                                               # ),
                                               # 
                                               # actionButton("create_auto_num_banding", 
                                               #              "Band", width = '15%',class='btn-primary',style='padding:4px; font-size:80%'),
                                      )
                          )
                 ),
                 # ~ MAIN TAB 2 - modelling ---------------------------------------------------------------------------------------------
                 tabPanel("Model", tags$style(type="text/css", "body {padding-top: 70px;}"),
                          sidebarLayout(
                            # ~ ~ SIDEBAR ------------------------------------------------------------------------------------------------------
                            sidebarPanel(
                              
                              fileInput("action_import_model", "Import the existing model (rds format)",
                                        width = '98%'
                              ),
                              downloadButton("action_export_model","Save the current model parameters",style = "width:98%;padding:4px; font-size:80%"),
                              br(),
                              br(),
                              pickerInput(
                                inputId = "FactorList", 
                                label = "Factor list", 
                                choices = list(), options = list(`actions-box` = TRUE), 
                                multiple = TRUE
                              ),
                              br(),
                              actionButton("action_fit_model", "Fit current model", width = '98%',class='btn-primary',style='padding:4px; font-size:80%'),
                              br(),
                              br(),
                              actionButton("action_set_ref_model", "Set as ref-model", width = '32%',class='btn-primary',style='padding:4px; font-size:80%'),actionButton("action_clear_ref_model", "Clear ref-model", width = '32%',class='btn-primary',style='padding:4px; font-size:80%'),actionButton("action_load_ref_model", "Load ref-model", width = '32%',class='btn-primary',style='padding:4px; font-size:80%'),
                              htmlOutput("output_model_fit_warning")
                              
                              # checkboxGroupInput("FactorList", "Factor list", choices = list())
                              ,width = 3
                              ,style = "position: fixed;width: 23%; height: 90vh; overflow-y: auto;"),
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          # ~ ~ SUB-TAB 2a - parameters ----------------------------------------------------------------------------
                                          tabPanel("Specify Parameter",
                                                   br(),
                                                   fluidRow(
                                                     column(6,
                                                            tags$h3("Specify Parameters"),
                                                            br(),
                                                            sliderInput("slider_max_depth",      "Max Depth"            , min = 1,     max = 15,  value = 6   , step = 1, width = '75%'),
                                                            sliderInput("slider_min_weight",     "Minimum Child Weight" , min = 0,     max = 2000, value = 200 , step = 20, width = '75%'   ),
                                                            sliderInput("slider_eta",            "Learning Rate"        , min = 0.005, max = 0.3, value = 0.15, step = 0.01, width = '75%'),
                                                            sliderInput("slider_subsample",      "Subsample"            , min = 0.05,  max = 1,   value = 0.75 , step = 0.05, width = '75%' ),
                                                            sliderInput("slider_early_stopping", "Early Stopping Rounds", min = 1,     max = 200, value = 10  , step = 1, width = '75%'    ),
                                                            sliderInput("slider_max_delta",      "Max Delta Step"       , min = 0,     max = 10,  value = 5   , step = 0.1, width = '75%'  ),
                                                            sliderInput("slider_alpha",          "Alpha"                , min = 0,     max = 5,   value = 0.3   , step = 0.1, width = '75%'  ),
                                                            sliderInput("slider_lambda",         "Lambda"               , min = 0,     max = 5,   value = 0.3   , step = 0.1, width = '75%'  ),
                                                            sliderInput("slider_gamma",          "Gamma"                , min = 0,     max = 5,   value = 2.8   , step = 0.1, width = '75%'  ),
                                                            sliderInput("slider_notrees",        "Number of Trees"      , min = 100,   max = 2000,   value = 200   , step = 100, width = '75%'  ),
                                                            sliderInput("slider_model_fit_seed", "Seed", min = 0, max = 500, value = 1, width = '75%')),
                                                     
                                                     column(6,
                                                            tags$h3("Monotonic Constraints"),
                                                            br(),
                                                            fluidRow(
                                                              column(6, checkboxGroupInput("checkbox_monotonic_increasing", "Increasing", choices = list())),
                                                              column(6, checkboxGroupInput("checkbox_monotonic_decreasing", "Decreasing", choices = list()))
                                                            ),
                                                     ),
                                                   ),
                                                   
                                                   
                                                   # fluidRow(
                                                   #   column(4, sliderInput("slider_max_depth",      "Max Depth"            , min = 1,     max = 15,  value = 6   , step = 1    )),
                                                   #   column(4, sliderInput("slider_min_weight",     "Minimum Child Weight" , min = 0,     max = 2000, value = 200 , step = 20   )),
                                                   #   column(4, sliderInput("slider_eta",            "Learning Rate"        , min = 0.005, max = 0.3, value = 0.15, step = 0.01)),
                                                   #   column(4, sliderInput("slider_subsample",      "Subsample"            , min = 0.05,  max = 1,   value = 0.75 , step = 0.05 )),
                                                   #   column(4, sliderInput("slider_early_stopping", "Early Stopping Rounds", min = 1,     max = 200, value = 10  , step = 1    )),
                                                   #   column(4, sliderInput("slider_max_delta",      "Max Delta Step"       , min = 0,     max = 10,  value = 5   , step = 0.2  )),
                                                   #   column(4, sliderInput("slider_alpha",          "Alpha"                , min = 0,     max = 5,   value = 0.3   , step = 0.1  )),
                                                   #   column(4, sliderInput("slider_lambda",         "Lambda"               , min = 0,     max = 5,   value = 0.3   , step = 0.1  )),
                                                   #   column(4, sliderInput("slider_gamma",          "Gamma"                , min = 0,     max = 5,   value = 2.8   , step = 0.1  )),
                                                   #   column(4, sliderInput("slider_notrees",        "Number of Trees"      , min = 100,   max = 2000,   value = 200   , step = 100  )),
                                                   #   column(8,sliderInput("slider_model_fit_seed", "Seed", min = 0, max = 500, value = 1))),
                                                   
                                                   # fluidRow(
                                                   #   column(4, 
                                                   #          selectInput("select_tuning_params", 
                                                   #                      "Parameters to tune", 
                                                   #                      choices = tuning_parameter_df$label, 
                                                   #                      multiple=TRUE, 
                                                   #                      selectize = FALSE,
                                                   #                      size = 9 )
                                                   #   ),
                                                   #   # column(4,
                                                   #   #        br(),
                                                   #   #        actionButton("action_tune_parameters", "Tune Parameters")
                                                   #   # )
                                                   # )
                                          ),
                                          # ~ ~ SUB-TAB 2b - model constraints ---------------------------------------------------------------------------
                                          # tabPanel("Constraints",
                                          #          br(),
                                          #          fluidRow(
                                          #            column(6, checkboxGroupInput("checkbox_monotonic_increasing", "Increasing", choices = list())),
                                          #            column(6, checkboxGroupInput("checkbox_monotonic_decreasing", "Decreasing", choices = list()))
                                          #          )
                                          # ),
                                          # ~ ~ SUB-TAB 2c - model visualisation ---------------------------------------------------------------------------
                                          tabPanel("Model Visualisation",
                                                   br(),
                                                   radioButtons("shap_value_property", "Choose Shap Calculation Method", choices = list("Approximate" = 2,"Normal" = 1), inline = TRUE),
                                                   actionButton("calculate_shap", "Calculate Shap Value For Model Visualisation",class='btn-primary'),
                                                   htmlOutput("output_shap_warning"),
                                                   br(),
                                                   fluidRow( column(6,selectInput("FactorSelect", "Factor select", choices = list(), width = '80%')),
                                                             column(6,selectInput("DataSelect", "Partition select", choices = list("Modelling" = 1, "Holdout" = 2, "Validation" = 3), width = '80%') )),
                                                   plotlyOutput("AvsEPlot",width = 1200, height = 600),
                                                   
                                                   downloadButton("export_avse_chart","Export the current chart (html)"),
                                                   
                                                   
                                                   br(),
                                                   br(),
                                                   sliderInput("slider_shap_sample", "Sample fraction for shap", min = 0.01, max = 1, value = 0.1, step = 0.01,width = '80%'),
                                                   radioButtons("radio_shap_chart_type", "Chart Type", choices = list("Normal" = 1, "Violin" = 2), inline = TRUE),
                                                   actionButton("action_plot_shap", "Plot Shap visualisation",class='btn-primary',width = '50%'),
                                                   plotlyOutput("shap_plot",width = 1000, height = 600),
                                                   
                                          ),
                                          # ~ ~ SUB-TAB 2d - model validation -------------------------------------------------------------------------------
                                          tabPanel("Model Validation",
                                                   br(),
                                                   radioButtons("radio_imp_type", "Importance type", choices = list("Base" = 1, "OHE" = 2), inline = TRUE),
                                                   plotlyOutput("importancePlot",width = 1200, height = 600),
                                                   selectInput("results_select_data", "Partition select", choices = list("Modelling" = 1, "Holdout" = 2, "Validation" = 3)),
                                                   textOutput("text_gini_score"),
                                                   plotlyOutput("gains_plot",width = 900, height = 600)
                                                   
                                          ),
                              )
                            )
                          )
                 ),
                 # ~ MAIN TAB 3 - Investigation -----------------------------------------------------------------------------------------
                 tabPanel("Investigation", tags$style(type="text/css", "body {padding-top: 70px;}"),
                          tabsetPanel(type = "tabs",
                                      # ~ ~ SUB-TAB 3a - 2 way actual vs expected ---------------------------------------------------------------------------------
                                      tabPanel("2 way Actual vs Expected", tags$style(type="text/css", "body {padding-top: 70px;}"),
                                               br(),
                                               sidebarLayout(
                                                 # ~ ~ SIDEBAR ------------------------------------------------------------------------------------------------------
                                                 sidebarPanel(
                                                   selectInput("explore_dataset", 'Select Partition', choices = list("Modelling" = 1, "Holdout" = 2, "Validation" = 3)),
                                                   selectInput("explore_factor_1", 'Select Factor - x axis', choices = list()),
                                                   selectInput("explore_factor_2", "Select Factor - series", choices = list()),
                                                   actionButton("action_plot_ave", "Plot the chart",class='btn-primary',width = '100%')
                                                 ),
                                                 mainPanel( plotlyOutput("explorer_ave_plot"),
                                                            radioButtons("chart_type", "Chart Type", choices = list("Normal", "100%"), inline = TRUE),
                                                            plotlyOutput("explorer_exp_plot"))
                                               )   
                                      ),
                          )
                 ),
                 # ~ MAIN TAB 4 - Save/download -----------------------------------------------------------------------------------------------
                 tabPanel("Save/Download", tags$style(type="text/css", "body {padding-top: 70px;}"),
                          br(),
                          downloadButton("download_prediction", "download model prediction"),
                          br(),
                          br(),
                          #dataTableOutput("test")
                 )
)

# SERVER -------------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  # ~ REACTIVE VALUES ------------------------------------------------------------------------------------------------------
  
  rv <- reactiveValues( data_loaded        = FALSE,
                        model_structure_loaded = FALSE,
                        data_prepared      = FALSE,
                        data_prepared_iter = 0,
                        raw_data           = data.frame(),
                        subset_data        = data.frame(),
                        factor_lookup_tb   = NULL,
                        factor_lookup_tb_orig   = NULL,
                        clean_data         = list(),
                        banded_data        = list(),
                        summary_lst        = list(),
                        actual_summary_lst = list(),
                        factor_modified    = FALSE,
                        model_fit          = FALSE,
                        ref_model          = FALSE,
                        model_specs        = list(),
                        shap_base_fac_lst  = list(),
                        offset_factor      = NULL,
                        time_factor        = NULL,
                        key_data           = list(),
                        pred_download      = data.frame(),
                        factor_information = data.frame(),
                        model_log          = data.frame() )
  
  # ~ MAIN TAB 1 - data manipulation ---------------------------------------------------------------------------------------
  # ~ ~ SUB-TAB 1a - Import the raw data -------------------------------------------------------------------------------------------
  output$output_import_warning <- renderText({ '<span style=\"color:red\">DATA NOT LOADED</span>'})
  
  observeEvent(input$action_read_file, {
    
    rv$raw_data <- readRDS(input$action_read_file$datapath)
    
    rv$full_var_list <- colnames(rv$raw_data)
    rv$num_var_list  <- colnames(select_if(rv$raw_data, is.numeric))
    
    updateSelectInput( session, 'select_response',         choices = rv$num_var_list )
    updateSelectInput( session, 'select_exposure',         choices = rv$num_var_list )
    updateSelectInput( session, 'select_time',             choices = c("NONE",rv$full_var_list) )
    updateSelectInput( session, 'select_offset',           choices = c("NONE",rv$num_var_list) )
    updateSelectInput( session, 'select_partition_factor', choices = rv$full_var_list )
    updateSelectInput( session, 'select_rating_factors',   choices = rv$full_var_list )
    updateSelectInput( session, 'select_policy_keys',   choices = c("NONE",rv$full_var_list ))
    
    output$output_import_warning <- renderText({ '<span style=\"color:green\">DATA LOADED SUCCESSFULLY</span>'})
    rv$data_loaded = TRUE
    
    output$file_info <- renderText({ paste0( "FILE SIZE: ", file.info(input$action_read_file$datapath)$size)})
    output$number_rows <- renderText ({ paste0( "NUMBER OF ROWS:", nrow(rv$raw_data))})
    output$number_cols <- renderText ({ paste0( "NUMBER OF COLUMNS:", ncol(rv$raw_data))})
    output$data_summary_desc <- renderTable({unclass(summary(rv$raw_data))})
    output$table_raw_data_head <- renderDataTable({rv$raw_data})
    
  })
  
  
  output$action_save_prep <- downloadHandler(
    filename = "model_structure.rds",
    content = function(file) {
      req(rv$data_loaded)
      saveRDS(list(input$select_model_type,
                   input$select_response,
                   input$select_exposure,
                   input$select_time,
                   input$select_offset,
                   input$select_data_partition,
                   input$slider_seed,
                   input$slider_cut_points,
                   input$select_partition_factor,
                   input$select_rating_factors,
                   input$select_policy_keys),
              file)
    }
  )
  
  
  
  
  observeEvent(input$action_import_prep, {
    req(rv$data_loaded)
    imported_prep <- readRDS(input$action_import_prep$datapath)
    updateSelectInput( session, 'select_model_type',         selected=imported_prep[[1]] )
    updateSelectInput( session, 'select_response',         selected=imported_prep[[2]] )
    updateSelectInput( session, 'select_exposure',         selected=imported_prep[[3]] )
    updateSelectInput( session, 'select_time',             selected=imported_prep[[4]] )
    updateSelectInput( session, 'select_offset',           selected=imported_prep[[5]] )
    updateSelectInput( session, 'select_data_partition',   selected=imported_prep[[6]] )
    updateSliderInput( session, 'slider_seed',             value=imported_prep[[7]] )
    updateSliderInput( session, 'slider_cut_points',       value=imported_prep[[8]] )
    updateSelectInput( session, 'select_partition_factor', selected=imported_prep[[9]] )
    updateSelectInput( session, 'select_rating_factors', selected=imported_prep[[10]] )
    updateSelectInput( session, 'select_policy_keys', selected=imported_prep[[11]] )
  })
  
  # ~ ~ SUB-TAB 1b - data prep ---------------------------------------------------------------------------------------------
  output$output_data_prep_warning <- renderText({ '<span style=\"color:red\">DATA NOT PREPARED</span>'})
  observeEvent(input$action_prepare_data, { 
    # only proceed if the data has been loaded correctly
    if( rv$data_loaded ){
      # only proceed if the partition is well defined
      if ( n_distinct( rv$raw_data[[input$select_partition_factor]] ) == 3 | input$select_data_partition == '1' ){
        rv$partition_type        <- input$select_data_partition
        rv$partition_slider_seed <- input$slider_seed
        rv$partition_slider_cut  <- input$slider_cut_points
        rv$partition_select      <- input$select_partition_factor
        rv$response_symbol       <- sym(input$select_response)
        rv$exposure_symbol       <- sym(input$select_exposure)
        if (input$select_offset == "NONE"){
          rv$offset_factor <- NULL
        } else{
          rv$offset_factor <- input$select_offset
        }
        if (input$select_time == "NONE"){
          rv$time_factor <- NULL
        } else{
          rv$time_factor <- input$select_time
        }
        
        rv$model_type      <- as.numeric( input$select_model_type )
        rv$model_specs     <- get_model_type( as.numeric( input$select_model_type ) )
        rv$sample_fraction <- input$slider_sample_fraction
        
        # drop any zero exposure rows and define partitions
        # select only needed columns
        subset_data <- rv$raw_data %>% filter( !!(rv$exposure_symbol) > 0 )
        
        if(as.numeric(input$select_data_partition)==1){
          subset_data <- subset_data %>% partition_data_seeded( input$slider_seed, input$slider_sample_fraction, input$slider_cut_points ) 
        } else {
          
          ifelse(input$select_policy_keys == "NONE",rv$key_data <- list(),rv$key_data <- subset_data[,c(input$select_policy_keys,input$select_partition_factor)] %>% partition_data_factor( input$select_partition_factor ))
          
          
          #output$test <- renderDataTable({rv$key_data[[1]]})
          subset_data <- subset_data %>% partition_data_factor( input$select_partition_factor ) 
        }
        rv$subset_data <- subset_data %>% map( select, input$select_response, input$select_exposure, input$select_rating_factors, rv$offset_factor, rv$time_factor)
        rv$subset_data_orig <- rv$subset_data
        rv$factor_list     <- rv$subset_data[[1]]   %>% colnames %>% setdiff(c(input$select_response, input$select_exposure, rv$offset_factor)) %>% sort()
        updateSelectInput( session, 'FactorSelect_oneway', choices = rv$factor_list )
        
        rv$actual_summary_lst   <-summarise_fun_actual(rbind(rv$subset_data[[1]],rv$subset_data[[2]],rv$subset_data[[3]]), rv$factor_list,input$select_exposure,input$select_response)
        rv$actual_summary_lst_orig <- rv$actual_summary_lst
        
        output$Actual_plot <- renderPlotly({ Actual_Plt(rv$actual_summary_lst,input$select_exposure,input$FactorSelect_oneway) })
        output$Actual_plot_orig <- renderPlotly({ Actual_Plt(rv$actual_summary_lst_orig,input$select_exposure,input$FactorSelect_oneway) })
        
        
        message("Data partitioned")
        rv$model_structure_loaded=TRUE
        output$output_data_prep_warning <- renderText({ '<span style=\"color:green\">DATA PREPARED SUCCESSFULLY</span>'})
        
      } else { message("ERROR: Please check your partition factor has only three levels") }
    } else { message("ERROR: No data found, please load data") }
  })
  
  observeEvent(input$action_modify_factor, {
    # rv$factor_lookup_tb = rv$actual_summary_lst[[input$FactorSelect_oneway]]
    if (as.numeric(input$mapping_method)==1){
      rv$factor_lookup_tb_orig <- data.frame(current_factor_level = rv$actual_summary_lst_orig[[input$FactorSelect_oneway]][['factor_name']], 
                                             new_factor_level = rv$actual_summary_lst_orig[[input$FactorSelect_oneway]][['factor_name']])
    }
    else{
      
      if (is.numeric(rv$subset_data_orig[[1]][[input$FactorSelect_oneway]])){
        
        valueband <- Hmisc::cut2(rbind(rv$subset_data_orig[[1]],rv$subset_data_orig[[2]],rv$subset_data_orig[[3]])[[input$FactorSelect_oneway]],g=input$select_bandings, onlycuts = TRUE)
        bandlevels <- as.numeric(levels(Hmisc::cut2(rbind(rv$subset_data_orig[[1]],rv$subset_data_orig[[2]],rv$subset_data_orig[[3]])[[input$FactorSelect_oneway]],g=input$select_bandings,levels.mean=TRUE)))
        
        rv$factor_lookup_tb_orig <- data.frame(current_factor_level_left = valueband[1:length(valueband)-1],
                                               current_factor_level_right = valueband[2:length(valueband)],
                                               new_factor_level = bandlevels)
        rv$factor_lookup_tb_orig[length(valueband)-1, 'current_factor_level_right'] <- rv$factor_lookup_tb_orig[length(valueband)-1, 'current_factor_level_right']+1
      } else {
        shinyalert(title = "No Auto banding available for categorical variable", type = "error")
      }
      
      
      
    }
    
    rv$factor_lookup_tb <- rv$factor_lookup_tb_orig 
    rv$factor_modified <- TRUE
    
    
  })  
  
  observeEvent(input$FactorSelect_oneway, {
    rv$factor_lookup_tb<-NULL
    rv$factor_modified <- FALSE
  })

  observe({
    shinyjs::hide("Modify")
    
    if(rv$factor_modified)
      shinyjs::show("Modify")
  })
  
  observe({
    shinyjs::hide("Pastemapping")
    
    if(rv$factor_modified)
      shinyjs::show("Pastemapping")
  })

  
  observeEvent(input$mapping_method, {
    if(input$mapping_method ==2){
      shinyjs::show("select_bandings")
    }else{
      shinyjs::hide("select_bandings")
    }
  })
  
  # observe({
  #   shinyjs::hide("select_bandings")
  #   
  #   if(as.numeric(input$mapping_method)==2)
  #     shinyjs::show("Pastemapping")
  # })
  # 
  
  # observe({
  #   shinyjs::hide("action_clear_modification")
  # 
  #   if(rv$factor_modified)
  #     shinyjs::show("action_clear_modification")
  # })

  # observe({
  #   shinyjs::hide("action_clear_current_modification")
  # 
  #   if(rv$factor_modified)
  #     shinyjs::show("action_clear_current_modification")
  # })

  
  
  
  output$factor_mapping_table <- DT::renderDataTable(server = FALSE,{
    DT::datatable(rv$factor_lookup_tb,editable = TRUE,
                  extensions=c("Buttons",'Scroller'),
                  options = list(dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 
                                             'excel'),
                                 deferRender = TRUE,
                                 scrollY = 400,
                                 scrollX =FALSE,
                                 scroller = TRUE)
    )
  })
  # output$factor_mapping_table <- renderDT({
  #   datatable(rv$factor_lookup_tb, editable = TRUE,extensions = c('Buttons','Scroller'), options = list(
  #     dom = 'Bfrtip',
  #     pageLength=1000,
  #     buttons = c('copy', 'csv', 'excel'),
  #     deferRender = TRUE,
  #     scrollY = 400,
  #     scrollX =FALSE,
  #     scroller = TRUE
  #   ))
  # })
  
  observeEvent(input$factor_mapping_table_cell_edit, {
    row  <- input$factor_mapping_table_cell_edit$row
    clmn <- input$factor_mapping_table_cell_edit$col
    rv$factor_lookup_tb[row, clmn] <- input$factor_mapping_table_cell_edit$value
  }) 
  
  observeEvent(input$Pastemapping, {
    rv$factor_lookup_tb<-read.table(file = "clipboard", sep = "\t", header=TRUE)
  })   
  
  
  observeEvent(input$Modify, {
    for(sample in as.numeric(c(1,2,3))) {
      
      if (as.numeric(input$mapping_method)==1){
        rv$subset_data[[sample]][[input$FactorSelect_oneway]] <-  with(rv$factor_lookup_tb,
                                                                       new_factor_level[match(rv$subset_data_orig[[sample]][[input$FactorSelect_oneway]],
                                                                                              current_factor_level)])
      } else {
        rv$subset_data[[sample]][[input$FactorSelect_oneway]] <- as.numeric(as.character(cut(rv$subset_data_orig[[sample]][[input$FactorSelect_oneway]], unique(c(rv$factor_lookup_tb$current_factor_level_left, rv$factor_lookup_tb$current_factor_level_right)), rv$factor_lookup_tb$new_factor_level, right = FALSE)))
      }
      
      
    }
    rv$actual_summary_lst[[input$FactorSelect_oneway]]<-summarise_fun_actual(rbind(rv$subset_data[[1]],rv$subset_data[[2]],rv$subset_data[[3]]), input$FactorSelect_oneway,input$select_exposure,input$select_response)[[input$FactorSelect_oneway]]
    
  })
  
  observeEvent(input$action_clear_current_modification, {
    for(sample in as.numeric(c(1,2,3))) {
      rv$subset_data[[sample]][[input$FactorSelect_oneway]]<-rv$subset_data_orig[[sample]][[input$FactorSelect_oneway]]
    }
    rv$actual_summary_lst[[input$FactorSelect_oneway]]<-summarise_fun_actual(rbind(rv$subset_data[[1]],rv$subset_data[[2]],rv$subset_data[[3]]), input$FactorSelect_oneway,input$select_exposure,input$select_response)[[input$FactorSelect_oneway]]
    
  })
  
  observeEvent(input$action_clear_modification, {
    rv$subset_data   <- rv$subset_data_orig
    rv$actual_summary_lst   <-summarise_fun_actual(rbind(rv$subset_data[[1]],rv$subset_data[[2]],rv$subset_data[[3]]), rv$factor_list,input$select_exposure,input$select_response)
    
  })
  
  output$output_modelling_data_warning <- renderText({ '<span style=\"color:red\">MODELLING DATA NOT CREATED</span>'})
  observeEvent(input$Prepare_modelling_data, {
    # clean data
    rv$clean_data_df <-  rv$subset_data %>% map( char_to_factor ) %>% map( clean_char_data )
    rv$clean_data <- rv$clean_data_df  %>% map( as.matrix )
    
    #For the shap plot later
    #rv$clean_data_df_train <- rv$clean_data[[1]]%>% as.data.frame()
    message("Data cleaned")
    
    # define factor info
    
    rv$factor_list_ohe <- rv$clean_data[[1]] %>% colnames %>% setdiff(c(input$select_response, input$select_exposure, rv$offset_factor)) %>% sort()
    
    # check to see if prep has been well defined
    if(length(rv$factor_list) > 0){
      #prepare the different datasets required
      rv$factor_information <- rv$subset_data[[1]] %>% collate_factor_information( rv$factor_list, rv$factor_list_ohe )
      
      message("Factor information extracted")
      
      rv$banded_data <- rv$subset_data    %>% map( band_data, rv$factor_information,input$select_bandings )
      rv$banded_data_train <- rv$banded_data[[1]]%>% as.data.frame()
      
      rv$summary_lst   <- rv$banded_data %>% map( summarise_fun, rv$factor_list, input$select_exposure,input$select_response)
      
      message("Data summarised")
      
      # update inputs
      updatePickerInput( session, 'FactorList', choices = rv$factor_list_ohe )
      updateSelectInput( session, 'FactorSelect', choices = rv$factor_list )
      updateCheckboxGroupInput( session, 'checkbox_monotonic_increasing', choices = rv$factor_list_ohe )
      updateCheckboxGroupInput( session, 'checkbox_monotonic_decreasing', choices = rv$factor_list_ohe )
      
      # plots for model development
      output$AvsEPlot <- renderPlotly({ AvsE_Plt(rv$summary_lst,input$DataSelect,input$select_exposure,input$FactorSelect) })
      
      
      # clear model log and update information
      rv$model_log <- data.frame()
      
      rv$data_prepared = TRUE
      message("COMPLETE: Data prepared successfully")
      output$output_modelling_data_warning <- renderText({ '<span style=\"color:green\">MODELLING DATA CREATED SUCCESSFULLY</span>'})
      rv$data_prepared_iter = rv$data_prepared_iter + 1
    } else { message("ERROR: Please check your factor specifications") }
    
    observeEvent(input$select_data_partition, {
      updateTabsetPanel(session, "data_partition_options", selected = switch(as.numeric(input$select_data_partition), "seeded_partition", "factor_partition"))
    })
  })
  
  observeEvent(input$create_auto_num_banding, {
    rv$factor_information$needs_banding = ifelse(rv$factor_information$levels>input$select_levels & rv$factor_information$numerical==TRUE,1,0)
    rv$banded_data <- rv$subset_data    %>% map( band_data, rv$factor_information,input$select_bandings )
    rv$banded_data_train <- rv$banded_data[[1]]%>% as.data.frame()
    
    rv$summary_lst   <- rv$banded_data %>% map( summarise_fun, rv$factor_list, input$select_exposure,input$select_response)
    message("COMPLETE: Data banded")
  }) 
  
  
  # ~ ~ SUB-TAB 2c - model visualisation -----------------------------------------------------------------------------
  
  
  
  observeEvent(input$action_import_model, {
    req(rv$data_loaded,rv$data_prepared)
    imported_model <- readRDS(input$action_import_model$datapath)
    
    updateSliderInput( session, 'slider_model_fit_seed',               min = 0,     max = 500, value = imported_model$model_seeds                                        )
    updatePickerInput(session, 'FactorList',                   choices = rv$factor_list_ohe,   selected = imported_model$fitted_factors                                  )
    
    ## Parameter Tuning
    
    
    updateSliderInput( session, 'slider_max_depth',                    min = 1,     max = 15,  value = imported_model$paremeters$max_depth[1],        step = 1     )
    updateSliderInput( session, 'slider_min_weight',                   min = 0,     max = 2000, value =imported_model$paremeters$min_child_weight[1], step = 20    )
    updateSliderInput( session, 'slider_eta',                          min = 0.005, max = 0.3, value = imported_model$paremeters$eta[1],              step = 0.01 )
    updateSliderInput( session, 'slider_subsample',                    min = 0.05,  max = 1,   value = imported_model$paremeters$subsample[1],        step = 0.05  )
    updateSliderInput( session, 'slider_early_stopping',               min = 1,     max = 200, value = imported_model$early_stopping[1],   step = 1     )
    updateSliderInput( session, 'slider_max_delta',                    min = 0,     max = 10,  value = imported_model$paremeters$max_delta_step[1],        step = 0.1   )
    updateSliderInput( session, 'slider_alpha',                        min = 0,     max = 5,   value = imported_model$paremeters$alpha[1],            step = 0.1   )
    updateSliderInput( session, 'slider_lambda',                       min = 0,     max = 5,   value = imported_model$paremeters$lambda[1],           step = 0.1   )
    updateSliderInput( session, 'slider_gamma',                        min = 0,     max = 5,   value = imported_model$paremeters$gamma[1],            step = 0.1   )
    updateSliderInput( session, 'slider_notrees',                      min = 100,     max = 2000,   value = imported_model$no_tress[1],                 step = 100   )
    ##Constraints
    updateCheckboxGroupInput(session, 'checkbox_monotonic_increasing', choices = rv$factor_list_ohe, selected = imported_model$monotonic_constraints_increasing )
    updateCheckboxGroupInput(session, 'checkbox_monotonic_decreasing', choices = rv$factor_list_ohe, selected = imported_model$monotonic_constraints_decreasing )
    
  })
  
  
  
  output$action_export_model <- downloadHandler(
    filename = "model_detail.rds",
    content = function(file) {
      req(rv$data_loaded,rv$model_fit,rv$model_structure_loaded, rv$data_prepared)
      to_save <- list(
        model_seed = isolate(rv$model_seed),
        fitted_factors = isolate(rv$f_list), 
        fitted_factors_base = isolate(rv$f_list_base),
        monotonic_constraints_increasing = input$checkbox_monotonic_increasing, 
        monotonic_constraints_decreasing = input$checkbox_monotonic_decreasing, 
        paremeters = isolate(rv$xgb_current_model$params),
        no_tress = input$slider_notrees,
        early_stopping = input$slider_early_stopping,
        model_seeds = input$slider_model_fit_seed
        # interaction_constraints = isolate(rv$int_constraint_sel),
        # model = isolate(rv$xgb_current_model)
      )
      saveRDS(to_save, file)
      
    }
  )
  
  
  observeEvent(input$action_set_ref_model, {
    if (rv$model_fit){
      rv$ref_model_detail <- list(
        model_seed = isolate(rv$model_seed),
        fitted_factors = isolate(rv$f_list), 
        fitted_factors_base = isolate(rv$f_list_base),
        monotonic_constraints_increasing = input$checkbox_monotonic_increasing, 
        monotonic_constraints_decreasing = input$checkbox_monotonic_decreasing, 
        paremeters = isolate(rv$xgb_current_model$params),
        no_tress = input$slider_notrees,
        early_stopping = input$slider_early_stopping,
        model_seeds = input$slider_model_fit_seed
        # interaction_constraints = isolate(rv$int_constraint_sel),
        # model = isolate(rv$xgb_current_model)
      )
      rv$summary_lst_ref = rv$summary_lst
      
      for(sample in as.numeric(c(1,2,3))) {
        for (fac in names(rv$summary_lst_ref[[sample]])){
          rv$summary_lst_ref[[sample]][[fac]] = rv$summary_lst_ref[[sample]][[fac]][,c("factor_name" ,"prediction","AW_shap_premium")]
          rv$summary_lst_ref[[sample]][[fac]] = rename(rv$summary_lst_ref[[sample]][[fac]], c(prediction_ref=prediction,AW_shap_premium_ref=AW_shap_premium))
        }
        
      }
      rv$ref_model=TRUE
    } else { message("ERROR: please fit model first") }
    
  }) 
  
  observeEvent(input$action_clear_ref_model, {
    output$AvsEPlot <- renderPlotly({ AvsE_Plt(rv$summary_lst,input$DataSelect,input$select_exposure,input$FactorSelect) })
    rv$ref_model=FALSE
    
  })   
  
  
  observeEvent(input$action_load_ref_model, {
    if (rv$ref_model){
      updateSliderInput( session, 'slider_model_fit_seed',               min = 0,     max = 500, value = rv$ref_model_detail$model_seeds                                        )
      updatePickerInput(session, 'FactorList',                   choices = rv$factor_list_ohe,   selected = rv$ref_model_detail$fitted_factors                                  )
      
      updateSliderInput( session, 'slider_max_depth',                    min = 1,     max = 15,  value = rv$ref_model_detail$paremeters$max_depth[1],        step = 1     )
      updateSliderInput( session, 'slider_min_weight',                   min = 0,     max = 2000, value =rv$ref_model_detail$paremeters$min_child_weight[1], step = 20    )
      updateSliderInput( session, 'slider_eta',                          min = 0.005, max = 0.3, value = rv$ref_model_detail$paremeters$eta[1],              step = 0.01 )
      updateSliderInput( session, 'slider_subsample',                    min = 0.05,  max = 1,   value = rv$ref_model_detail$paremeters$subsample[1],        step = 0.05  )
      updateSliderInput( session, 'slider_early_stopping',               min = 1,     max = 200, value = rv$ref_model_detail$early_stopping[1],   step = 1     )
      updateSliderInput( session, 'slider_max_delta',                    min = 0,     max = 10,  value = rv$ref_model_detail$paremeters$max_delta_step[1],        step = 0.2   )
      updateSliderInput( session, 'slider_alpha',                        min = 0,     max = 5,   value = rv$ref_model_detail$paremeters$alpha[1],            step = 0.1   )
      updateSliderInput( session, 'slider_lambda',                       min = 0,     max = 5,   value = rv$ref_model_detail$paremeters$lambda[1],           step = 0.1   )
      updateSliderInput( session, 'slider_gamma',                        min = 0,     max = 5,   value = rv$ref_model_detail$paremeters$gamma[1],            step = 0.1   )
      updateSliderInput( session, 'slider_notrees',                      min = 100,     max = 2000,   value = rv$ref_model_detail$no_tress[1],                 step = 100   )
      ##Constraints
      updateCheckboxGroupInput(session, 'checkbox_monotonic_increasing', choices = rv$factor_list_ohe, selected = rv$ref_model_detail$monotonic_constraints_increasing )
      updateCheckboxGroupInput(session, 'checkbox_monotonic_decreasing', choices = rv$factor_list_ohe, selected = rv$ref_model_detail$monotonic_constraints_decreasing )
      
    } else { message("ERROR: please set the reference model first") }
    
  })   
  
  # ~ ~ ~ OBSERVE EVENT: fit model button pressed ----
  output$output_model_fit_warning <- renderText({ '<span style=\"color:red\">MODEL NOT FIT</span>'})
  observeEvent(input$action_fit_model, {
    # only proceed if the data has been loaded and prepared correctly
    if( rv$data_loaded & rv$model_structure_loaded & rv$data_prepared ){
      # only proceed if at least one factor has been selected
      if( length(input$FactorList) > 0 ){
        # set factor list so that charts are not reactive
        rv$f_list <- input$FactorList
        rv$f_list_base <- rv$f_list %>% as.data.frame() %>% setDT() %>% left_join( rv$factor_information, by = c( '.' = 'ohe_feature' ) ) %>% select(.,base_feature)
        #define modelling seed 
        rv$model_seed <- input$slider_model_fit_seed
        set.seed(input$slider_model_fit_seed)
        # prepare the data
        rv$prepared_data <- prep_xgboost_data(rv$clean_data, rv$f_list, as.character(rv$response_symbol), as.character(rv$exposure_symbol), rv$offset_factor, rv$time_factor)
        # get monotonic constraints (note if checked in both lists no constraint will apply)
        mon_constraints <- paste0("(",
                                  paste(as.character(
                                    as.numeric(input$FactorList %in% input$checkbox_monotonic_increasing) -
                                      as.numeric(input$FactorList %in% input$checkbox_monotonic_decreasing)
                                  ), collapse = ","),
                                  ")")
        # get parameters for model from sliders
        parameter_list <- list(subsample            = input$slider_subsample,
                               max_depth            = input$slider_max_depth,
                               min_child_weight     = input$slider_min_weight,
                               eta                  = input$slider_eta,
                               max_delta_step       = input$slider_max_delta,
                               alpha                = input$slider_alpha,       
                               lambda               = input$slider_lambda,      
                               gamma                = input$slider_gamma,
                               monotone_constraints = mon_constraints)
        # fit model
        rv$xgb_current_model <- fit_xgboost_model(rv$prepared_data, parameter_list, input$slider_early_stopping, input$slider_notrees,rv$model_specs)
        message("COMPLETE: Model Fit successfully")
        
        # do not return anything from trivial models (single tree) as errors arise
        if (rv$xgb_current_model$best_iteration > 1){
          # generate predictions
          returned_predictions <- return_xgboost_predictions(rv$banded_data, rv$prepared_data, rv$xgb_current_model, rv$response_symbol, rv$exposure_symbol)
          
          rv$banded_data        <- returned_predictions[[1]]
          rv$gains_data         <- returned_predictions[[2]]
          rv$gains_data_optimal <- returned_predictions[[3]]
          rv$summary_lst        <- rv$banded_data %>% map( summarise_with_pred_fun, rv$factor_list, input$select_exposure,input$select_response,'xgb_prediction')
          
          # get importance values for ohe factors
          rv$imp_table_ohe <- xgb.importance(rv$f_list,model=rv$xgb_current_model)
          # convert to importance table for base factors
          rv$imp_table_base <- rv$imp_table_ohe %>% append_base_features( rv$factor_information )
          
          if (rv$ref_model) {
            rv$summary_lst_comb=rv$summary_lst
            for(sample in as.numeric(c(1,2,3))) {
              for (fac in names(rv$summary_lst_ref[[sample]])){
                rv$summary_lst_comb[[sample]][[fac]] =  left_join(rv$summary_lst_comb[[sample]][[fac]], rv$summary_lst_ref[[sample]][[fac]],
                                                                  c("factor_name" = "factor_name"))
              }
              
            }
            output$AvsEPlot <- renderPlotly({ AvsE_with_ref_Plt(rv$summary_lst_comb,input$DataSelect,input$select_exposure,input$FactorSelect) })
          } else {
            output$AvsEPlot <- renderPlotly({ AvsE_Plt(rv$summary_lst,input$DataSelect,input$select_exposure,input$FactorSelect) })
          }
          
          message("COMPLETE: Prediction Returned")
          
          
          
          
          
          output$importancePlot <- renderPlotly({ggplotly(xgb.ggplot.importance(switch(as.numeric( input$radio_imp_type ),rv$imp_table_base,rv$imp_table_ohe)) + theme_bw()) })
          output$gains_plot <- renderPlotly({ ggplotly(plot_gains(input$results_select_data, rv$gains_data, rv$gains_data_optimal) + theme_bw()) })          
          message("COMPLETE: AvsE Returned")
          
          gini_scores <- calculate_gini_score(rv$gains_data) / calculate_gini_score(rv$gains_data_optimal)
          output$text_gini_score <- renderText({ paste0( "GINI SCORE: ", round(gini_scores[[as.numeric(input$results_select_data)]]*100,2),"%" ) })
          
          
          # update explorer tab selections
          updateSelectInput( session, 'explore_factor_1', choices = rv$factor_list )
          updateSelectInput( session, 'explore_factor_2', choices = rv$factor_list )
          
          # clear the explorer tab
          
          output$explorer_ave_plot     <- NULL
          output$explorer_exp_plot     <- NULL
          output$explorer_exp_plot_100 <- NULL
          output$shap_plot <- NULL
          updateNumericInput(session, 'axis_min', value = 0)
          updateNumericInput(session, 'axis_max', value = 0)
          updateSelectInput( session, 'ShapFactorSelect', choices =list() )
          
          
          output$output_model_fit_warning <- renderText({ '<span style=\"color:green\">MODEL FIT SUCCESSFULLY</span>'})
          rv$model_fit = TRUE 
          
          
          message("COMPLETE: Model fit successfully and metrics calculated")         
          
        } else { message("ERROR: Only one tree in model, please adjust parameters") }
      } else { message("ERROR: Please select at least one rating factor") }
    } else { message("ERROR: Please check data load and preparation") }
  })  
  
  
  
  
  
  # Shap visualisation ---------------------------------------------------------------------------------
  
  
  output$output_shap_warning <- renderText({ '<span style=\"color:red\">Shap not calculatre</span>'})
  observeEvent(input$calculate_shap, {
    if (rv$model_fit ) {
      if (as.numeric(input$shap_value_property) == 1) {
        rv$shap_contributions <- predict(rv$xgb_current_model, rv$prepared_data[[1]], predcontrib = TRUE, approxcontrib = FALSE)
      } else {
        rv$shap_contributions <- predict(rv$xgb_current_model, rv$prepared_data[[1]], predcontrib = TRUE, approxcontrib = TRUE)
      }      
      
      rv$shap_contributions_df <- rv$shap_contributions  %>% as.data.frame()
      
      for(fac in rv$imp_table_base$Feature) {
        
        if (fac %in% colnames(rv$shap_contributions_df )){
          shap_base_fac <- rv$shap_contributions_df [[fac]]
        } else {
          shap_base_fac <- rowSums(select(rv$shap_contributions_df, starts_with(fac)))
        }
        
        rv$shap_base_fac_lst[[fac]]<-data.frame(factor_name=rv$banded_data_train[[fac]],shap_val=shap_base_fac,exposure=rv$banded_data_train[[input$select_exposure]])
        rv$shap_base_fac_lst[[fac]]$W_shap <- rv$shap_base_fac_lst[[fac]]$shap_val* rv$shap_base_fac_lst[[fac]]$exposure
        summary_shap_base_fac <- rv$shap_base_fac_lst[[fac]] %>% group_by(factor_name) %>%
          summarise(exposure = sum(exposure),
                    W_shap = sum(W_shap)) %>%
          mutate(AW_shap=W_shap/exposure +1)
        
        for(sample in as.numeric(c(1,2,3))) {
          rv$summary_lst[[sample]][[fac]] <- left_join(select(rv$summary_lst[[sample]][[fac]],-c(AW_shap)), summary_shap_base_fac[,c("factor_name", "AW_shap")], 
                                                       c("factor_name" = "factor_name"))
          rv$summary_lst[[sample]][[fac]][['AW_shap_premium']] <- rv$summary_lst[[sample]][[fac]][['AW_shap']] * rv$summary_lst[[sample]][[fac]][['AW_prediction']]
          
        }
      }
      
      if (rv$ref_model) {
        
        rv$summary_lst_comb=rv$summary_lst
        for(sample in as.numeric(c(1,2,3))) {
          for (fac in names(rv$summary_lst_ref[[sample]])){
            rv$summary_lst_comb[[sample]][[fac]] <-  left_join(rv$summary_lst_comb[[sample]][[fac]], rv$summary_lst_ref[[sample]][[fac]], 
                                                               c("factor_name" = "factor_name"))
          }
          
        }
        output$AvsEPlot <- renderPlotly({ AvsE_with_ref_Plt(rv$summary_lst_comb,input$DataSelect,input$select_exposure,input$FactorSelect) })
      } else {
        output$AvsEPlot <- renderPlotly({ AvsE_Plt(rv$summary_lst,input$DataSelect,input$select_exposure,input$FactorSelect) })
      }
      
      
      
      rv$shap_output <- rv$clean_data[[1]] %>% as.data.frame %>% 
        select(!!(rv$exposure_symbol),!!(rv$response_symbol)) %>% 
        cbind(rv$shap_contributions)   
      
      message("COMPLETE: Shap contribution Calculated") 
      output$output_shap_warning <- renderText({ '<span style=\"color:green\">Shap value calculated successfully</span>'})
      
      # this is only plotted on the training set
      
    }
  })   
  
  observeEvent(input$action_plot_shap, {
    if (rv$model_fit ) {
      rv$shap_select_factor <- input$FactorSelect
      
      if ( rv$shap_select_factor %in% rv$imp_table_base$Feature){
        rv$shap_base_fac_sample_lst[[rv$shap_select_factor]] <- rv$shap_base_fac_lst[[rv$shap_select_factor]] %>% sample_frac(as.numeric(input$slider_shap_sample))
        if (as.numeric(input$radio_shap_chart_type) == 1) {
          output$shap_plot <- renderPlotly({ggplotly(ggplot(rv$shap_base_fac_sample_lst[[rv$shap_select_factor]], aes(factor(factor_name), shap_val))+theme(axis.text.x = element_text(angle = 45, hjust = 1)) + geom_point(colour = "blue") + labs(x = rv$shap_select_factor, y ='Shap_values'))})
        } else {
          output$shap_plot <- renderPlotly({ggplotly(ggplot(rv$shap_base_fac_sample_lst[[rv$shap_select_factor]], aes(factor(factor_name), shap_val))+theme(axis.text.x = element_text(angle = 45, hjust = 1)) + geom_point(colour = "blue") + geom_violin(colour = "black")+ geom_jitter(height = 0, width = 0.01,colour = "blue")+labs(x = rv$shap_select_factor, y ='Shap_values'))})
          
        }
      } else {
        output$shap_plot <- NULL
      }
    }
    
  })
  
  
  output$export_avse_chart <- downloadHandler(
    filename = function() {
      paste(input$FactorSelect, ".html", sep = "")
    },
    content = function(file) {
      req(rv$data_loaded)
      saveWidget(as_widget(AvsE_Plt(rv$summary_lst,input$DataSelect,input$select_exposure,input$FactorSelect)), file, selfcontained = TRUE)
    }
  )
  
  
  # ~ ~ SUB-TAB 3a - 2 way actual vs expected ----------------------------------------------------------------------------------
  observeEvent(input$action_plot_ave, { 
    
    if( input$explore_factor_1 != input$explore_factor_2) {
      explore_sym      <- sym(input$explore_factor_1) 
      explore_time_sym <- sym(input$explore_factor_2)
      
      rv$explorer_ave_data <- rv$banded_data[[as.numeric(input$explore_dataset)]] %>% 
        group_by(!!explore_sym, !!explore_time_sym) %>%
        summarise(reponse_sum = sum(as.numeric(!!rv$response_symbol)),
                  response_prediction = sum(as.numeric(xgb_prediction)),
                  exposure_sum = sum(as.numeric(!!rv$exposure_symbol))) %>%
        mutate(actual = reponse_sum / exposure_sum,
               prediction = response_prediction / exposure_sum)
      
      rv$explorer_ave_data[[input$explore_factor_2]] = factor(rv$explorer_ave_data[[input$explore_factor_2]])
      rv$explorer_ave_data[[input$explore_factor_1]] = factor(rv$explorer_ave_data[[input$explore_factor_1]])
      
      ave_min <- min(pmin(rv$explorer_ave_data$actual, rv$explorer_ave_data$prediction))
      ave_max <- max(pmax(rv$explorer_ave_data$actual, rv$explorer_ave_data$prediction))
      updateNumericInput(session, 'axis_min', value = ave_min)
      updateNumericInput(session, 'axis_max', value = ave_max)
      
      level_names <- rv$explorer_ave_data %>% pull(input$explore_factor_2) %>% unique()
      
      
      explorer_ave_plot <- ggplot(data = rv$explorer_ave_data, aes_string(x = input$explore_factor_1, group = input$explore_factor_2, colour = input$explore_factor_2 )) +
        geom_line( aes(y = actual)) +
        geom_line( aes(y = prediction),  linetype = "dashed")
      
      output$explorer_ave_plot <- renderPlotly({ggplotly(explorer_ave_plot+ coord_cartesian(ylim = c(input$axis_min, input$axis_max),default=TRUE) + theme_bw()) })
      
      ggbase_exp <- ggplot(data = rv$explorer_ave_data, aes_string(x = input$explore_factor_1, group = input$explore_factor_2, fill = input$explore_factor_2 ))
      explorer_exp_plot_normal <- ggplotly(ggbase_exp + geom_col( aes(y = exposure_sum), colour = "black") + theme_bw())
      explorer_exp_plot_100 <- ggplotly(ggbase_exp + geom_col( aes(y = exposure_sum), colour = "black", position = "fill") + theme_bw())
      
      observeEvent(input$chart_type, {
        if (input$chart_type == 'Normal') {
          output$explorer_exp_plot <- renderPlotly({explorer_exp_plot_normal})
        } else {
          output$explorer_exp_plot <- renderPlotly({explorer_exp_plot_100})
        }
        
      })
      
      updateCheckboxGroupInput(session, 'factor_2_levels', choices = level_names, selected = level_names)
      
    } else {message("ERROR: please select two different factors")}
  })
  
  
  # ~ MAIN TAB 4 - Download session ---------------------------------------------------------------------------------------
  
  
  
  output$download_prediction <- downloadHandler(
    filename = "prediction_data.csv",
    content = function(file) {
      ifelse(input$select_policy_keys =='NONE',rv$pred_download <- rbind(rv$banded_data[[1]],rv$banded_data[[2]],rv$banded_data[[3]]),rv$pred_download <-rbind(cbind(rv$banded_data[[1]],rv$key_data[[1]]),cbind(rv$banded_data[[2]],rv$key_data[[2]]),cbind(rv$banded_data[[3]],rv$key_data[[3]])))
      write.csv(rv$pred_download,file, row.names = FALSE)
      
    }
  )
  
  
  
  
  
  
  observeEvent(c(input$select_model_type,
                 input$select_response,
                 input$select_exposure,
                 input$select_data_partition,
                 input$slider_seed,
                 input$slider_cut_points,
                 input$select_partition_factor,
                 input$select_rating_factors,
                 input$slider_sample_fraction,
                 input$select_policy_keys), {
                   if (rv$model_structure_loaded == TRUE) {
                     output$output_data_prep_warning <- renderText({ '<span style=\"color:orange\">MODELSTRUCTURE CHANGED - RECOMMEND LOADING STRUCTURE AGAIN TO AVOID UNEXPECTED RESULTS</span>'})
                   }
                   if (rv$data_prepared == TRUE) {
                     output$output_modelling_data_warning <- renderText({ '<span style=\"color:orange\">MODELSTRUCTURE CHANGED - RECOMMEND PREPARING MODELLING DATA AGAIN TO AVOID UNEXPECTED RESULTS</span>'})
                   }
                   if (rv$model_fit == TRUE){
                     output$output_model_fit_warning <- renderText({ '<span style=\"color:orange\">MODELSTRUCTURE CHANGED - RE-FIT THE MODEL</span>'})
                     output$output_shap_warning <- renderText({ '<span style=\"color:orange\">MODELSTRUCTURE CHANGED - RE-Calculate Shap</span>'})
                     #output$shap_plot <- NULL
                   }
                     
                     
                   
                 }) 
  observeEvent(c(input$action_modify_factor), {
                   if (rv$data_prepared == TRUE) {
                     output$output_modelling_data_warning <- renderText({ '<span style=\"color:orange\">DATA MAPPING CHANGED - RECOMMEND PREPARING MODELLING DATA AGAIN TO AVOID UNEXPECTED RESULTS</span>'})
                   }
                   if (rv$model_fit == TRUE) {
                     output$output_model_fit_warning <- renderText({ '<span style=\"color:orange\">DATA MAPPING CHANGED - RE-FIT THE MODEL</span>'})
                     output$output_shap_warning <- renderText({ '<span style=\"color:orange\">DATA MAPPING CHANGED - RE-Calculate Shap</span>'})
                     #output$shap_plot <- NULL
                   }
                   
                 })
  
  observeEvent(c(input$FactorList,
                 input$slider_seed,
                 input$slider_subsample,
                 input$slider_max_depth,
                 input$slider_min_weight,
                 input$slider_eta,
                 input$slider_max_delta,
                 input$slider_alpha,
                 input$slider_lambda,
                 input$slider_gamma,
                 input$slider_notrees,
                 input$checkbox_monotonic_increasing,
                 input$checkbox_monotonic_decreasing), {
                   if (rv$model_fit == TRUE) {
                     output$output_model_fit_warning <- renderText({ '<span style=\"color:orange\">FACTORS OR PARAMETER CHANGED - RE-FIT THE MODEL</span>'})
                     output$output_shap_warning <- renderText({ '<span style=\"color:orange\">FACTORS OR PARAMETER CHANGED - RE-Calculate Shap</span>'})
                   }
                 })  
  
  
}  



shinyApp(ui = ui, server = server)



