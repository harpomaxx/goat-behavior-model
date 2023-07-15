# Goat Behavior Detection Model

## Overview

This repository hosts the machine learning model that has been developed for differentiating between *grazing mimosa* and other activities (resting, walking, and grazing) in goats using sensor data. The model, a gradient boosting algorithm, is built with 15 selected features and demonstrates high accuracy in detecting goat grazing activities.

Machine learning models are becoming increasingly popular for detecting animal behaviors. However, their application to real-world scenarios has often been hindered due to their complexity and lack of explainability. To overcome this limitation, this model leverages the Boruta feature selection algorithm and the SHAP interpretation technique, promoting both the model's interpretability and performance.

## Key Features

-   **Boruta Feature Selection**: This algorithm is used to select the most relevant features for our machine learning model, enhancing the accuracy of the model.

-   **SHAP Interpretation**: SHAP (SHapley Additive exPlanations) is a unified measure of feature importance that assigns each feature an importance value for a particular prediction. This technique provides a highly effective way of interpreting the decisions of the machine learning model.

-   **Gradient Boost Algorithm**: The model is built using a gradient boost algorithm. This decision tree-based ensemble Machine Learning algorithm is a robust and powerful technique that can capture complex hierarchical patterns in the data.

## Results and Implications

The model has proved extremely accurate in detecting grazing activities. More importantly, the application of the Boruta and SHAP techniques have greatly improved the model's explainability, identifying potential weaknesses and errors, thereby laying a path for future improvements.

Moreover, the simplicity of the resulting model not only reduces computational complexity and processing time but also enhances interpretability, thus facilitating the deployment in real-life scenarios.

## Model Usage

Please refer to the Jupyter notebook `goat_behavior_prediction` for an overview and usage of this model.

## Shiny App

There also a minimal shiny app where you can try the model.
The shiny app is available at [Hugging Face](https://huggingface.co/spaces/harpomaxx/goat-behavior) :hugginface:

## Contribute

This is an open-source project. Contributions are always welcome!

## License

Please see the [LICENSE](./LICENSE) file for more information.
