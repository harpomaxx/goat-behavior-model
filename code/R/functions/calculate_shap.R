suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(fastshap)) # for fast (approximate) Shapley values
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(doMC))

registerDoMC(cores = 10)


p_function_G <-
  function(object, newdata)
    caret::predict.train(object, newdata = newdata, type = "prob")[, "G"]
p_function_GM <-
  function(object, newdata)
    caret::predict.train(object, newdata = newdata, type = "prob")[, "GM"]
p_function_R <-
  function(object, newdata)
    caret::predict.train(object, newdata = newdata, type = "prob")[, "R"]
p_function_W <-
  function(object, newdata)
    caret::predict.train(object, newdata = newdata, type = "prob")[, "W"]

# DEPRECATED
calculate_shap_deprecated <- function(dataset,model,nsim=10) {
#  library(doParallel)
#  registerDoParallel(8)
  
  trainset <- dataset %>%  na.omit() %>%
    as.data.frame()
  trainset_y <- dataset %>%
    select(Activity) %>%
    na.omit() %>%
    unlist() %>%
    unname()
  trainset <- trainset %>% select(-Activity)
  trainset_G <- trainset[which(trainset_y == "G"), ]
  trainset_GM <- trainset[which(trainset_y == "GM"), ]
  trainset_R <- trainset[which(trainset_y == "R"), ]
  trainset_W <- trainset[which(trainset_y == "W"), ]
  
  
  # Compute fast (approximate) Shapley values using 50 Monte Carlo repetitions
  message(" - Calculating SHAP values for class G")
  shap_values_G <-
    fastshap::explain(
      model,
      X = trainset,
      pred_wrapper = p_function_G,
      nsim = nsim,
      newdata = trainset_G,
      .parallel = TRUE
    )
  message(" - Calculating SHAP values for class GM")
  shap_values_GM <-
    fastshap::explain(
      model,
      X = trainset,
      pred_wrapper = p_function_GM,
      nsim = nsim,
      newdata = trainset_GM,
      .parallel = TRUE
    )
  message(" - Calculating SHAP values for class R")
  shap_values_R <-
    fastshap::explain(
      model,
      X = trainset,
      pred_wrapper = p_function_R,
      nsim = nsim,
      newdata = trainset_R,
      .parallel = TRUE
    )
  message(" - Calculating SHAP values for class W")
  shap_values_W <-
    fastshap::explain(
      model,
      X = trainset,
      pred_wrapper = p_function_W,
      nsim = nsim,
      newdata = trainset_W,
      .parallel = TRUE
    #  adjust = TRUE
    )
  
 shap_values_GM$class<-"GM"
 shap_values_G$class<-"G"
 shap_values_R$class<-"R"
 shap_values_W$class<-"W" 
  
 shap_values<-rbind(shap_values_G,
                     shap_values_GM,
                     shap_values_R,
                     shap_values_W)
 shap_values
}


#' A new function for calcualting SHAP values
#' the function returns a dataframe with SHAP values in the same
#' order of the original dataset.
#' 
#' SHAP value dataframe also contains information about Animal and 
#' the prediction of the model. Notice that SHAP are calculated considering
#' the class (ground truth) and not the prediction. The prediction column is only
#' used for filtering ana analysis. The function `calculate_shapp_class()` can be
#' used for calculating SHAP values on prediction
#' 
#' @param dataset a dataset used for calcuating SHAP. The dataset is used for 
#' permutation during SHAP calculation and also each class is filtered and SHAP 
#' value for each class is calculated.
#' @param model a model 
#' @param nsim number of monte carlo simulation
#'
#' @return
#' @export
#'
#' @examples
calculate_shap <- function(dataset,model,nsim=10) {
  trainset <- dataset %>%  na.omit() %>%
    as.data.frame()
  trainset_y <- dataset %>%
    select(Activity) %>%
    na.omit() %>%
    unlist() %>%
    unname()
  ## Create an ID for maintaining the order
  trainset <- cbind(id=seq(1:nrow(trainset)), trainset)
  trainset <- trainset %>% select(-Activity)
  
  trainset_G <- trainset[which(trainset_y == "G"), ]
  trainset_GM <- trainset[which(trainset_y == "GM"), ]
  trainset_R <- trainset[which(trainset_y == "R"), ]
  trainset_W <- trainset[which(trainset_y == "W"), ]
  
  id <- c(trainset_G$id, 
          trainset_GM$id, 
          trainset_R$id,
          trainset_W$id)
  trainset <- trainset %>% select(-id)
  trainset_G <- trainset_G %>% select(-id)
  trainset_GM <- trainset_GM %>% select(-id)
  trainset_R <- trainset_R %>% select(-id)
  trainset_W <- trainset_W %>% select(-id)
  
  Anim <- c(trainset_G$Anim, 
            trainset_GM$Anim, 
            trainset_R$Anim,
            trainset_W$Anim)
  trainset <- trainset %>% select(-Anim)
  trainset_G <- trainset_G %>% select(-Anim)
  trainset_GM <- trainset_GM %>% select(-Anim)
  trainset_R <- trainset_R %>% select(-Anim)
  trainset_W <- trainset_W %>% select(-Anim)
  
  predictions <- c(trainset_G$predictions, 
                   trainset_GM$predictions, 
                   trainset_R$predictions,
                   trainset_W$predictions)
  trainset <- trainset %>% select(-predictions)
  trainset_G <- trainset_G %>% select(-predictions)
  trainset_GM <- trainset_GM %>% select(-predictions)
  trainset_R <- trainset_R %>% select(-predictions)
  trainset_W <- trainset_W %>% select(-predictions)
  
  # Compute fast (approximate) Shapley values using 50 Monte Carlo repetitions
  message(" - Calculating SHAP values for class G")
  shap_values_G <-
    fastshap::explain(
      model,
      X = trainset,
      pred_wrapper = p_function_G,
      nsim = nsim,
      newdata = trainset_G,
      .parallel = TRUE
    )
  message(" - Calculating SHAP values for class GM")
  shap_values_GM <-
    fastshap::explain(
      model,
      X = trainset,
      pred_wrapper = p_function_GM,
      nsim = nsim,
      newdata = trainset_GM,
      .parallel = TRUE
    )
  message(" - Calculating SHAP values for class R")
  shap_values_R <-
    fastshap::explain(
      model,
      X = trainset,
      pred_wrapper = p_function_R,
      nsim = nsim,
      newdata = trainset_R,
      .parallel = TRUE
    )
  message(" - Calculating SHAP values for class W")
  shap_values_W <-
    fastshap::explain(
      model,
      X = trainset,
      pred_wrapper = p_function_W,
      nsim = nsim,
      newdata = trainset_W,
      .parallel = TRUE
      #  adjust = TRUE
    )
  
  shap_values_G$class<-"G"
  shap_values_GM$class<-"GM"
  shap_values_R$class<-"R"
  shap_values_W$class<-"W" 
  
  shap_values<-rbind(shap_values_G,
                     shap_values_GM,
                     shap_values_R,
                     shap_values_W)
  
  shap_values <- shap_values %>% tibble::add_column(Anim)
  shap_values <- shap_values %>% tibble::add_column(predictions)
  #shap_values <-shap_values %>% tibble::add_column(id)
  shap_values[order(id),] 
}

#' Calculate SHAP values for a given PREDICTED class
#'
#' @param dataset the dataset used for permutation during SHAP calculation
#' @param new_data the new data we want to calculate SHAP
#' @param model  the model used for explanation
#' @param nsim  the number of Monte Carlos Simulations
#' @param function_class a wrapper function to obtain only a particular class
#' @param class_name the name of the class
#'
#' @return
#' @export
#'
#' @examples
#' 
#' # Calculate the SHAP values for class G on new data
#' shap_values_G <- calculate_shap_class(
#' dataset, 
#' new_data = newdata, 
#' model = goat_model 
#' nsim = 100, 
#' function_class = p_function_G,
#' class_name = "G")
#' 
#' 
calculate_shap_class <- function(dataset, new_data, model,nsim=10, 
                                 function_class, class_name = "G") {
  trainset <- dataset %>%  na.omit() %>%
    as.data.frame()
  trainset_y <- dataset %>%
    select(predictions) %>%
    na.omit() %>%
    unlist() %>%
    unname()
  
  trainset<- trainset %>%select (-Activity,-predictions,-Anim)
  new_data_class <- new_data 
  
  Anim <- new_data_class$Anim 
  new_data_class <- new_data_class %>% select(-Anim)
  
  Activity <- new_data_class$Activity
  new_data_class <- new_data_class %>% select(-Activity)
  
  predictions <- new_data_class$predictions
  new_data_class <- new_data_class %>% select(-predictions)
  
  # Compute fast (approximate) Shapley values using 50 Monte Carlo repetitions
  message(" - Calculating SHAP values for class ",class_name)
  shap_values_class <-
    fastshap::explain(
      model,
      X = trainset,
      pred_wrapper = function_class,
      nsim = nsim,
      newdata = new_data_class,
      .parallel = TRUE
    )
  
  shap_values_class$class<-Activity
  shap_values<-shap_values_class
  
  shap_values <- shap_values %>% tibble::add_column(Anim)
  shap_values <- shap_values %>% tibble::add_column(predictions)
  shap_values
}

shap_summary_plot<-function(shap_values){
  summary_plot <-
    shap_values %>% reshape2::melt() %>% group_by(class, variable) %>% 
    summarise(mean = mean(abs(value))) %>% 
    arrange(desc(mean)) %>%
    ggplot() +
    ggdark::dark_theme_classic() +
    geom_col(aes(
      y = variable,
      x = mean,
      group = class,
      fill = class
    ), position = "stack") +
    xlab("Mean(|Shap Value|) Average impact on model output magnitude")
  summary_plot
  
}

shap_beeswarm_plot<-function(shap_values,dataset){
  
  shap_values <- shap_values %>% reshape2::melt()
  dataset<-dataset %>% mutate(class=Activity) %>% select(-Activity) %>% 
    reshape2::melt() %>% group_by(variable) %>% 
    mutate(value_scale=range01(value))
  
  beeswarm_plot<-cbind(shap_values, feature_value=dataset$value_scale) %>% 
    # filter(class=="GM") %>%
    ggplot()+
    facet_wrap(~class)+
    #ggdark::dark_theme_bw()+
    theme_classic()+
    geom_hline(yintercept=0, 
               color = "red", size=0.5)+
    ggforce::geom_sina(aes(x=variable,y=value,color=feature_value),size=0.5,bins=4,alpha=0.9,shape=15)+
    scale_colour_gradient(low = "yellow", high = "red", na.value = NA)+
    scale_colour_gradient(low = "skyblue", high = "orange", na.value = NA)+
    xlab("Feature")+ylab("SHAP value")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  beeswarm_plot
}