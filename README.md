# Security Characteristic Line Web App: 
## https://alex-hl.shinyapps.io/scl_app/

* This is calculator app that was built in R using the Shiny apps and tidyquant packages (stock API)
* The app allows the user to see which publicly traded security generated alpha (5Y monthly basis)
* The app also computes the measure of systematic risk, beta, for the chosen stock
* A plot of the security characteristic line is outputted. It is an interactive plot. The user can hover his/her cursor over any observations to see specific returns of the security vs the market on a given date
* Every time the user press computes, the most recent data about the chosen security is retrived automatically.
* The user can also chose which treasury bill he would like to use as the risk free rate in the model (data retrieved from the FRED)
