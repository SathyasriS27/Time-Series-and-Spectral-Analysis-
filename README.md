# Time-Series-and-Spectral-Analysis 

# Analysing historical forced migrations across the Atlantic Ocean 🚢
## Aim 🔍
The aim of this project involves the analyses the data, removing trends and seasonal effects, identifying the underlying process, understanding the dominant frequencies, and using the residuals to make predictions. 

## Introduction 🌊
Having given a collection of records documenting slave voyages across the Atlantic Ocean between 1654 and 1807, our primary goal is to uncover insights through careful analysis. First, we must address trends or seasonality in the data as they might overshadow the underlying variations. Trends represent long-term movement or patterns that show a general direction in which the data moves over time, while seasonality refers to a pattern of variation in the data that occurs at regular intervals.

When the trends and seasonality are removed, we can go on to identify the underlying process. We use tools like Correlograms and Yule-Walker Equations that help us analyse the autocorrelation or the correlation between data points at different time intervals. By examining the autocorrelation, we can get more insights into the underlying process and its characteristics. To further understand the underlying process, we use Periodograms; this helps us identify dominant frequencies in the data, representing the repeating patterns or cycles within the data.

Once we have the process that generated the data, we can utilize the residuals – the difference between the actual data and the fitted model, to make prediction values.

## Dataset used 
The dataset used in this analysis is the [slavery.Rdata](https://github.com/SathyasriS27/Time-Series-and-Spectral-Analysis-/blob/main/slavery.RData)

## Built with 🛠️
* R 

## Documentation 📚
In order to get the complete analysis and conclusion, please refer to this [documentation](https://github.com/SathyasriS27/Time-Series-and-Spectral-Analysis-/blob/main/201749908-Sathyasri%20Sudhakar-MATH5802.pdf).

