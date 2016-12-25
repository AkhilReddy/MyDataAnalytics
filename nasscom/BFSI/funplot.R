library(ggplot2)

comp<-function(predicted,actual){
    qplot(x=predicted,y=actual)+
        stat_smooth() +
        geom_abline(intercept = 0, slope = 1, color = "red")+
        labs(x = "predicted", y = "actual")
}



res_plot<-function(fit){
    df.fit<-fortify(fit)
    ggplot(data = df.fit, aes(x = .fitted, y = .resid)) +
        geom_point() + 
        stat_smooth() +
        labs(x = "fitted", y = "residual")+
        geom_abline(intercept = 0, slope = 0, color = "red") 
    #   identify(x = .fitted, y = .resid)
}

res_plot1<-function(fit,x){
    df.fit<-fortify(fit)
    ggplot(data = df.fit, aes(x , y = .resid)) +
        geom_point() + 
        stat_smooth() +
        geom_abline(intercept = 0, slope = 0, color = "red") 
        labs(x = deparse(substitute(x)), y = "residual")
  #  identify(x,.resid)
}

res_plot2<-function(predicted,actual){
    qplot(x = predicted, y =actual-predicted)+
        stat_smooth()+
        geom_abline(intercept = 0, slope = 0, color = "red") +
        labs(x = "fitted", y = "residual")
   # identify(predicted,actual)
}

test<-function(fit){
    plot(residuals(fit),data$index)#autocorrelation
    durbinWatsonTest(fit)#autocorrelation
    car::vif(fit)#collinearity
    ncvTest(fit)#heteroscedasticity
    residualPlots(fit,ask=T,plot=F)#non-linearity
    
    
}


dia<-function(fit){
    
    library(car)    
    layout(1)
    plot(fit)#diagnosis
    influencePlot(fit)#outliers
    residualPlots(fit,ask=T,layout=c(2,2),
                  type="rstandard")#non-linearity

}

RMS<-function(x,y,p){
    rms<-sqrt(( sum( (x - y)^2 , na.rm = TRUE )) / (length(x)-p-1)) 
    avg_error<-sum(x - y)/(length(x)-p)
    return(list(rms,avg_error))
}
