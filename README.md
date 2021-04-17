# Master-Thesis
Master's thesis : Ecole Supérieure de Commerce de Tunis - Manouba University -
M2 : Econometrie et Analyses Quantitatives

Macroeconomic Uncertainty: measure for Tunisia using Jurado et al. (2014) methodology

Reference:
Measuring Uncertainty: - Kyle Jurado, Sydney Ludvigson and Serena Ng
September 2014. American Economic Review 2015, 105(3): 1177–1216
http://dx.doi.org/10.1257/aer.20131193


**** A ******
Files to create macro-uncertainty index using data 2000:01-2020:06

Steps:

1. Prepare TNdata_SA_Stationary.xlsx
     Data for 101 Macro Series in Sheet 1 from TNdata.xlsx
     Add monthly Unemployment data the result of Temporal disaggregation of Anual Unemployment data
     output:  generateTN_data_SA_Stationary.RData
2. Generate_ferrors.m
      Estimate factors from 102 series (See step1 how add disaggregated Unemployment serie)
      Compute forecast errors for each of the factors using AR(4) model
      Compute forecast errors for each of the 102 macro series using factor-augmented regression
      output: vyt.txt ,vft.txt  and ferrors.RData
3. Estimate stochastic volatility in the forecast errors using 'stochvol' (R package):
     generate_svfdraws.R -> svfmeans2020.txt
     generate_svydraws.R -> svymeans2020.txt
4. Use svfmeans2020.txt and svymenas2020.txt as input to produce 102 uncertainty estimates
    generate_ut.R 
5. Aggregate ut into macro uncertainty
    generate_aggu.R-> .RData   (also includes the time series of uncertainty for each of the 102series, and for h=1..12)
    
    
*** B ****    
Figures:

Figure 1: plot_aggu.R

**** Data ***

The macro data are collected from The International Labor Organization: ILO,
Central Bank of Tunisia: BCT, the Tunis Stock Exchange: BVMT,
International Monetary Fund: IMF and the Tunisian National Institute of Statistics : INS
