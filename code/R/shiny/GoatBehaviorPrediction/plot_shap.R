suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggExtra))
suppressPackageStartupMessages(library(forcats))



range01 <- function(x){(x-min(x))/(max(x)-min(x))}

shap_summary_plot<-function(shap_values){
  summary_plot <-
    shap_values %>% reshape2::melt() %>% group_by(class, variable) %>% 
    summarise(mean = mean(abs(value))) %>% 
    arrange(desc(mean)) %>%
    ggplot() +
   # ggdark::dark_theme_classic() +
    theme_classic()+
    geom_col(aes(
      y = variable,
      x = mean,
      group = class,
      fill = class
    ), position = "stack") +
    ylab("Feature")+
    xlab("Mean(|Shap Value|) Average impact on model output magnitude per activity.")+
    guides(fill=guide_legend(title="Activity"))
  summary_plot
  
}


shap_summary_plot_perclass<-function(shap_values, class="G",color="#F8766D"){
  shap_values <-shap_values %>% as.data.frame() %>% filter(class == {{class}} )
  summary_plot <-
    shap_values %>% reshape2::melt() %>% group_by(variable) %>% 
    summarise(mean = mean(abs(value))) %>% 
    ggplot() +
    theme_classic()+
    geom_col(aes(
      x = mean,
      y = fct_reorder(variable,mean)
    ),
    fill = color
    ) +
    ylab("Feature")+
    xlab(paste0("Mean(|Shap Value|) Average impact on model output magnitude for activity ", class))+
    guides(fill=guide_legend(title="Activity"))
  summary_plot
  
}


shap_beeswarm_plot<-function(shap_values,dataset){
  
  shap_values <- shap_values %>% reshape2::melt()
  dataset<-dataset %>% mutate(class=Activity) %>% select(-Activity) %>% 
    reshape2::melt() %>% group_by(variable) %>% 
    mutate(value_scale=range01(value))
  
  beeswarm_plot<-cbind(shap_values, feature_value=dataset$value_scale) %>% # filter(class=="GM") %>%
    ggplot()+
    facet_wrap(~class)+
    #ggdark::dark_theme_bw()+
    theme_classic()+
    geom_hline(yintercept=0, 
               color = "red", size=0.5)+
    ggforce::geom_sina(aes(x=variable,y=value,fill=feature_value),color="black", size=2.4,bins=4,alpha=0.9,shape=22)+
    scale_fill_gradient(low = "yellow", high = "red", na.value = NA)+
    scale_fill_gradient(low = "skyblue", high = "orange", na.value = NA)+
    xlab("Feature")+ylab("SHAP value")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  beeswarm_plot
  
  
}


#' Dependency plot for a particular feature. The plot considers 
#' activities and FP/TP
#'
#' @param feature a particular feature to calculate
#' @param dataset a dataset with goat information
#' @param shap  a shap value dataset for each feature.
#'
#' @return a dependency plot for each activity considering the selected feature
#' @export ggplot object
#'
#' @examples
#' 
#' dataset <-
#' readr::read_delim("data/split/seba-caprino_loocv.tsv", 
#'        delim = '\t')
#' selected_variables <-
#'  readr::read_delim(
#'    "data/topnfeatures/seba-caprino_selected_features.tsv",
#'    col_types = cols(),
#'    delim = '\t'
#'  )
#' dataset <-
#'  dataset %>% select(selected_variables$variable, 
#'  Anim, 
#'  Activity)
#' goat_model <- readRDS("models/boost/seba-caprino_model.rds")
#' shap_values <- calculate_shap(dataset, 
#'                model = goat_model,
#'                nsim = 30)
#' dependency_plot_full(feature = "Steps",
#'                      dataset = dataset,
#'                      shap = shap_values) 

dependency_plot <- function(feature, dataset, shap) {
  newdata <- dataset %>% mutate({{ feature }} := range01(!!sym(feature)))
  #activities <- c("G", "GM", "W", "R")
  activities<-dataset %>% pull(Activity) %>% unique()
  plots <- list()
  for (activity in activities) {
    s <- shap[which(shap$class == activity), 1:18]
    x <- newdata[which(newdata$Activity == activity), ]
    data <- cbind(
      shap = (s %>% as.data.frame %>% select(feature)),
      feature = (x %>% select(feature)),
      tp = x %>% mutate(tp = ifelse(Activity == predictions, "TP", "FP")) %>% 
        pull(tp)
    )
    names(data) <- c("shap", "feature", "tp")
    p <- ggplot(data, aes(x = feature)) +
      geom_point(aes(y = shap, color = tp), alpha = 0.3, size = 0.8) +
      geom_smooth(aes(y = shap),
                  se = FALSE,
                  size = 0.5,
                  linetype = "dashed") +
      geom_hline(
        yintercept = 0,
        color = 'red',
        size = 0.5,
        alpha = 0.5
      ) +
      xlab(feature) +
      labs(title = paste0("Activity ", activity)) +
      ylab("SHAP Value") +
      ylim(-0.1, 0.4) +
      xlim(0, 1) +
      theme_light() +
      theme(legend.position = 'none')
    
    p1 <-
      ggMarginal(
        p,
        type = "histogram",
        fill = 'gray',
        color = 'white',
        size = 10,
        xparams = list(bins = 25),
        yparams = list(bins = 15)
      ) #,margins='x')
    plots[[activity]] <- p1
  }
  #plots
  do.call(grid.arrange, c(plots, ncol = 4))
}


#' Dependency plot for a particular feature on a particular animal. 
#' The plot considers activities and FP/TP
#'
#' @param feature a particular feature to calculate
#' @param dataset a dataset with goat information
#' @param shap  a shap value dataset for each feature.
#' @param anim the id of the animal
#' @return a dependency plot for each activity considering the selected feature
#' @export ggplot object
#'
#' @examples
#' 
#' dataset <-
#' readr::read_delim("data/split/seba-caprino_loocv.tsv", 
#'        delim = '\t')
#' selected_variables <-
#'  readr::read_delim(
#'    "data/topnfeatures/seba-caprino_selected_features.tsv",
#'    col_types = cols(),
#'    delim = '\t'
#'  )
#' dataset <-
#'  dataset %>% select(selected_variables$variable, 
#'  Anim, 
#'  Activity)
#' goat_model <- readRDS("models/boost/seba-caprino_model.rds")
#' shap_values <- calculate_shap(dataset, 
#'                model = goat_model,
#'                nsim = 30)
#' dependency_plot_anim(feature = "Steps",
#'                      dataset = dataset,
#'                      shap = shap_values,
#'                      anim = 'a13') 
dependency_plot_anim<- function(feature,dataset,shap,anim){
  
  newdata <- dataset %>% mutate({{feature}} := range01(!!sym(feature)))
  plots<-list()
  activities<-newdata %>% filter(Anim == anim) %>% pull(Activity) %>% unique()
  for (activity in activities) {
    s <- shap[which(shap$class == activity &
                      shap$Anim == anim
    ), 1:18]
    x <- newdata[which(newdata$Activity == activity &
                         newdata$Anim == anim
    ),]
    data <- cbind(shap=(s %>% as.data.frame %>% select(feature)), 
                  feature = (x %>% select(feature)), 
                  tp = x %>% mutate(tp=ifelse(Activity == predictions,"TP","FP")) %>% pull(tp) )
    names(data)<-c("shap","feature","tp")
    
    p <- ggplot(data, aes(x = feature)) +
      geom_point(aes(y = shap, color = tp), alpha = 0.3, size = 1.8) +
      geom_smooth(aes(y = shap),
                  se = FALSE,
                  size = 0.5,
                  linetype = "dashed") +
      geom_hline(
        yintercept = 0,
        color = 'red',
        size = 0.5,
        alpha = 0.5
      ) +
      xlab(feature) +
      labs(title = paste0("Activity ", activity)) +
      ylab("SHAP Value") +
      ylim(-0.1, 0.4) +
      xlim(0, 1) +
      theme_light() +
      theme(legend.position = 'none')
    
    p1 <-
      ggMarginal(
        p,
        type = "histogram",
        fill = 'gray',
        color = 'white',
        size = 15,
        xparams = list(bins = 25),
        yparams = list(bins = 15)
      ) #,margins='x')
    plots[[activity]] <- p1
  }
  do.call(grid.arrange, c(plots, ncol = length(activities)))
}

#' contribution plot for SHAP  values
#'
#' @param shap shap values for a particular class, animal, etc. 
#' @param num_row the row number of the observation to show
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' 
#' shap_values_G <- calculate_shap_class(
#' dataset = dataset, 
#' new_data = newdata, 
#' model= model, 
#' nsim = 100, 
#' function_class = p_function_G,
#' class_name ="G")
#' p1 <- contribution_plot(shap_values_G,num_row = 1) + 
#' labs(title="Anim a13: class G (FN)", subtitle = "SHAP analysis for class G") 
#' 
contribution_plot <-function(s, num_row = 1){
  s<-s[num_row,]
  s <- data.frame(
    Variable = names(s[,1:15]),
    Importance = apply(s[,1:15], MARGIN = 2, FUN = function(x) sum(x))
  )
  ggplot(s, aes(Variable, Importance, Importance,fill=Importance) )+
    geom_col() +
    coord_flip() +
    xlab("") +
    ylab("Shapley value")+
    theme_classic()+
    theme(legend.position = 'none')
}


contribution_plot_w_feature <-function(s, f, num_row = 1){
  d <- data.frame(
    variable = names(s[num_row,1:15]),
    importance = apply(s[num_row,1:15], MARGIN = 2, FUN = function(x) sum(x)),
    value = apply(f[num_row,1:15], MARGIN = 2, FUN = function(x) sum(x))
  )
  ggplot(d, aes(variable, importance, value,fill=value) )+
    geom_col() +
    geom_text(aes(label=round(value,digits = 2),hjust = 1.0),size=2)+
    coord_flip() +
    xlab("") +
    ylab("Shapley value")+
    scale_fill_gradient(low = 'lightgray', high = 'skyblue')+
    theme_classic()+
    theme(legend.position = 'none')
}
