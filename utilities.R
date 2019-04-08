normalize <- function(variable){
  variable / sum(variable)
}

scaler <- function(s, ref){
  (s - mean(ref)) / sd(ref)
}

unscaler <- function(s, ref){
  (s * sd(ref)) + mean(ref)
}

calc_intervals <- function(newdata, model){
  link <- fitted(model, newdata = newdata) %>% 
    as.tibble() %>% 
    bind_cols(newdata)
  
  predictions <- predict(model, newdata = newdata) %>% 
    as.tibble() %>% 
    bind_cols(newdata)
  list(link = link, predictions = predictions)
}


plot_intervals <- function(interval, x = ~year){
  list(
    geom_ribbon(data = interval$link, 
                aes_(x = x, ymin= ~Q2.5, ymax = ~Q97.5, label = NULL),
                alpha = 0.2
    ),
    geom_ribbon(data = interval$predictions,
                aes_(x = x, ymin=~Q2.5, ymax = ~Q97.5, label = NULL),
                alpha = 0.2
    )
  )
}

seq_range <- function(range_from_var, ...){
  range_ <- range(range_from_var)
  seq(range_[1], range_[2], ...)
}

prediction_observed_plot <- function(df, data, x, lbl = ~Location){
  df %>% 
    as_tibble() %>% 
    bind_cols(data) %>% 
    ggplot(aes_(x, ~Estimate, label = lbl))+
    geom_point(size =1.5, alpha = 3/4)+
    geom_abline(linetype=2, size = 0.5)+
    geom_linerange(aes_(ymax = ~Q97.5, ymin = ~Q2.5), size = 1/4)+
    # geom_linerange(aes_(ymax = (~Estimate) + (~Est.Error), ymin = (~Estimate) - (~Est.Error)), size = 1/2, color = "firebrick4")+
    labs(x = "Observed", y = "Predicted")+
    theme_bw()+
    theme(panel.grid = element_blank())
}

slope_plot <- function(model, size = 100){
  data <- model$data
  x <- model$formula[[1]][[2]] 
  y <- model$formula[[1]][[3]][[3]]
  model_coef <- model %>% 
    posterior_samples
  
  data %>% 
    ggplot(aes_(y,x))+
    geom_point(shape=1)+
    geom_abline(data = model_coef[1:size,], 
                aes(intercept = b_Intercept, slope = b_N),
                alpha=1/10
    )+
    theme_bw()
}

prior_post <- function(model, parameters){
# for ulam of rethinking
  
  par(mfrow=c(1, length(parameters)))
  posteriors <- extract.samples(model, pars = parameters)
  priors <- extract_prior_ulam(model, pars = parameters)
  
  for (parameter in parameters){
    
    d_post <- density(unlist(posteriors[[parameter]]))
    plot(d_post, main = parameter)
    
    
    d_prior <- density(unlist(priors[[parameter]]))
    lines(d_prior, lty = 2)
    
    # title(parameter)
  }
}


