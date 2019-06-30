# Jags-Ydich-XnomSsubj-MbernBetaOmegaKappa.R 
# Accompanies the book:
#   Kruschke, J. K. (2014). Doing Bayesian Data Analysis: 
#   A Tutorial with R, JAGS, and Stan. 2nd Edition. Academic Press / Elsevier.
#===============================================================================

genMCMC2 <- function( data , sName="s" , zName="z" ,  NName = "N",beta_params,gamma_params,
                    numSavedSteps=50000 , saveName=NULL,
                    thinSteps=1 ) { 
  require(rjags)
  #require(runjags)
  #-----------------------------------------------------------------------------
  # THE DATA.
  # N.B.: This function expects the data to be a data frame, 
  # with one component named z being a vector of effective alerts,
  # and one component named N being a vector of alerts
  # and one component named s being a factor of subject identifiers.
  z = as.numeric(data[,zName])
  N = as.numeric(data[,NName])
  s = as.numeric(data[,sName]) # ensures consecutive integer levels
  # Do some checking that data make sense:
  if ( any( z > N) ) { stop(" No of effective alerts cannot be greater than No of alerts") }
  Ntotal = sum(N)
  Nsubj = length(unique(s))
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(
    z = z ,
    N = N ,
    Nsubj = Nsubj
  )
  #-----------------------------------------------------------------------------
  # THE MODEL.
  modelString = CreateModelString(beta_params,gamma_params) # close quote for modelString
  #writeLines( modelString , con="TEMPmodel.txt" )
  #-----------------------------------------------------------------------------

  # INTIALIZE THE CHAINS.
  # Initial values of MCMC chains based on data:
  
  initsList = function() {
    thetaInit = rep(0,Nsubj)
    obs_eff = mydata$z/mydata$N
    thetaInit = rbinom(rep(1,Nsubj),N,obs_eff)/N
    thetaInit = 0.001+0.998*thetaInit # keep away from 0,1
    meanThetaInit = mean( thetaInit )
    kappaInit = 100 # lazy, start high and let burn-in find better value
    return( list( theta=thetaInit , omega=meanThetaInit , 
                  kappaMinusTwo=kappaInit-2 ) )
  }
  
  
  
  #-----------------------------------------------------------------------------
  # RUN THE CHAINS
  parameters = c( "theta","omega","kappa") # The parameters to be monitored
  adaptSteps = 500             # Number of steps to adapt the samplers
  burnInSteps = 500            # Number of steps to burn-in the chains
  nChains = 4                  # nChains should be 2 or more for diagnostics 
  nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains )
  

  # Create, initialize, and adapt the model:
  jagsModel = jags.model(textConnection(modelString) , data=dataList , inits=initsList , 
                         n.chains=nChains , n.adapt=adaptSteps )
  # Burn-in:
  cat( "Burning in the MCMC chain...\n" )
  update( jagsModel , n.iter=burnInSteps )
  # The saved MCMC chain:
  cat( "Sampling final MCMC chain...\n" )
  codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                              n.iter=nIter , thin=thinSteps )
  # resulting codaSamples object has these indices: 
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]

  return( codaSamples )


} # end function


#===============================================================================

smryMCMC2 = function(  codaSamples , compVal=0.5 , rope=NULL , 
                      diffIdVec=NULL , compValDiff=0.0 , ropeDiff=NULL , 
                      saveName=NULL ) {
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  Ntheta = length(grep("theta",colnames(mcmcMat)))
  summaryInfo = NULL
  rowIdx = 0
  # overall omega:
  summaryInfo = rbind( summaryInfo , 
                       summarizePost( mcmcMat[,"omega"] ,
                                      compVal=compVal , ROPE=rope ) )
  rowIdx = rowIdx+1
  rownames(summaryInfo)[rowIdx] = "omega"
  # kappa:
  summaryInfo = rbind( summaryInfo , 
                       summarizePost( mcmcMat[,"kappa"] ,
                                      compVal=NULL , ROPE=NULL ) )
  rowIdx = rowIdx+1
  rownames(summaryInfo)[rowIdx] = "kappa"
  # individual theta's:
  for ( tIdx in 1:Ntheta ) {
    parName = paste0("theta[",tIdx,"]")
    summaryInfo = rbind( summaryInfo , 
      summarizePost( mcmcMat[,parName] , compVal=compVal , ROPE=rope ) )
    rowIdx = rowIdx+1
    rownames(summaryInfo)[rowIdx] = parName
  }
  # differences of individual theta's:
  if ( !is.null(diffIdVec) ) {
    Nidx = length(diffIdVec)
    for ( t1Idx in 1:(Nidx-1) ) {
      for ( t2Idx in (t1Idx+1):Nidx ) {
        parName1 = paste0("theta[",diffIdVec[t1Idx],"]")
        parName2 = paste0("theta[",diffIdVec[t2Idx],"]")
        summaryInfo = rbind( summaryInfo , 
          summarizePost( mcmcMat[,parName1]-mcmcMat[,parName2] ,
                         compVal=compValDiff , ROPE=ropeDiff ) )
        rowIdx = rowIdx+1
        rownames(summaryInfo)[rowIdx] = paste0(parName1,"-",parName2)
      }
    }
  }
  # save:
  if ( !is.null(saveName) ) {
    write.csv( summaryInfo , file=paste(saveName,"SummaryInfo.csv",sep="") )
  }
  show( summaryInfo )
  return( summaryInfo )
}

#===============================================================================

plotMCMC2 = function( codaSamples , data , sName="s" , zName="z" ,  NName = "N" , 
                     compVal=0.02 , rope=NULL , thetaParams = c(1) ) {
  #-----------------------------------------------------------------------------
  # N.B.: This function expects the data to be a data frame, 
  # with one component named y being a vector of integer 0,1 values,
  # and one component named s being a factor of subject identifiers.
  z = as.numeric(data[,zName])
  N = as.numeric(data[,NName])
  s = as.numeric(data[,sName]) # ensures consecutive integer levels
  # Now plot the posterior:
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  chainLength = NROW( mcmcMat )
  # Plot omega, kappa:
  #openGraph(width=15,height=6.0*2)
  #par( mfrow=c(1,2) )
  #par( mar=c(3.5,1,3.5,1) , mgp=c(2.0,0.7,0) )
  
  postInfo = plotPost( mcmcMat[,"kappa"] , compVal=NULL , ROPE=NULL ,
                       xlab=bquote(kappa) , main="" , 
                       xlim=c( min(mcmcMat[,"kappa"]),
                               quantile(mcmcMat[,"kappa"],probs=c(0.990)) ) )
  
  postInfo = plotPost( mcmcMat[,"omega"] , compVal=compVal , ROPE=rope ,
                       xlab=bquote(omega) , main="Group Mode" ,
                       xlim=quantile(mcmcMat[,"omega"],probs=c(0.005,0.995)) )

  # Plot individual theta's :

  par( mfrow=c(1,1) )
  if ( !is.null(thetaParams) ) {
    Nidx = length(thetaParams)
    for ( tIdx in 1:Nidx ) {
        openGraph(height=5,width=7)
        parName = paste0("theta[",thetaParams[tIdx],"]")
       
          par( mar=c(3.5,1,1,1) , mgp=c(2.0,0.7,0) , pty="m" )
          postInfo = plotPost( mcmcMat[,parName] , cex.lab = 1.75 , 
                               compVal=compVal , ROPE=rope , cex.main=1.5 ,
                               xlab=parName , main="" )
          dataPropor <- z[s==thetaParams[tIdx]]/N[s==thetaParams[tIdx]] 
          points( dataPropor , 0 , pch="+" , col="red" , cex=3 )
        
                            }
  }
  
}
#===============================================================================
#===============================================================================

######################Function to prepare the MCMC sampled dataset for regression fitting##########
prep_data <- function(mcmc_sample,no_of_bands=10,sample_size=1000){
  mcmcMat = as.matrix(mcmc_sample,chains=TRUE)
  Theta_mat <- mcmcMat[,paste0('theta[',c(1:no_of_bands),']')]
  8##Sample a 1000 values from each band
  N_obs <- nrow(Theta_mat)
  Theta_mat <- Theta_mat[sample(1:N_obs,sample_size,replace= TRUE),]
  ##Convert to data frame
  Theta_df <- data.frame(cbind(c(1:sample_size),Theta_mat))
  colnames(Theta_df)[1] <- 'N'
  Theta_df_long <- gather(Theta_df,Band,Effectiveness,-N)
  ###Create candidate x axis value repesenting dsitance from threshold
  Theta_df_long$X <- rep(c(0:(no_of_bands-1)),each=sample_size)+ as.vector(replicate(no_of_bands,runif(sample_size)))
  
  return(Theta_df_long)
}

###Function to estimate the trend of effectiveness using frequentist regression######
get_freq_trend <- function(data){
  linear_trend <- lm(Effectiveness ~ X,data = data)
  quadratic_trend <- lm(Effectiveness ~ I(X^2),data=data)
  cat('Summary of Linear Fit: \n')
  print(summary(linear_trend))
  cat('\n Confidence intervals for slope: \n')
  print(confint(linear_trend,'X'))
  cat('\n Summary of Quadratic fit: \n')
  print(summary(quadratic_trend))
  cat('Confidence intervals for quadratic fit: \n ')
  print(confint(quadratic_trend,'I(X^2)'))
  cat('\n')
  cat('AIC  Comparison: \n')
  print(AIC(linear_trend,quadratic_trend))
  ggplot(data,aes(x = X , y=Effectiveness))+ 
    geom_smooth(method = 'lm',se=TRUE,aes(col='blue'))+
    geom_smooth(method = 'lm',formula = y ~ I(x^2),aes(col='green'))+
    labs(x='Distance from threshold')+
    ylim(0,0.1)+
    scale_color_manual(name='',values=c("blue","green"),labels=c("Linear Trend","Quadratic trend"))
  
}

######################unction to fit bayesian linear and quadratic models###############

fit_bayes_models <- function(data,n_chains,n_iter,n_warmup){
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores() - 1)
  
  cat('Starting Linear model fit...\n')
  stan_linear <- stan_glm(Effectiveness~X,data = data,
                          prior_intercept = normal(location = 0.02,scale=0.015,autoscale = FALSE),
                          prior = normal(location = 0, scale = 2.5), #Default prior
                          chains = n_chains, iter = n_iter ,warmup = n_warmup)
  
  cat('Completed Linear model.Starting Quadratic model fit... \n')
  
  stan_quadratic <- stan_glm(Effectiveness~ I(X^2),data = data,
                             prior_intercept = normal(location = 0.02,scale=0.015,autoscale = FALSE),
                             prior = normal(location = 0, scale = 2.5), #Default prior
                             chains = n_chains, iter = n_iter,warmup =n_warmup)
  cat('Completed  Quadratic model. \n')
  return(list(stan_linear,stan_quadratic))
  
}

#########Function to analyze bayesian fits######

analyze_bayes_models <- function(stan_linear,stan_quadratic,data_for_plot,n_bands){
  
  #####Downsample data for plotting
  sample <- data_for_reg %>% sample_n(1000)
  data_for_reg2 <- bind_rows(linear = sample, quadratic = sample,.id='type')
  
  draws_linear <- as.matrix(stan_linear)
  draws_quadratic <- as.matrix(stan_quadratic)
  
  cat('Summary of Linear Fit: \n')
  print(summary(stan_linear))
  cat('\n 95% Highest density intervals for slope and intercept parameters: \n')
  #posterior_interval(draws_linear,pars = c('X','(Intercept)'))
  slope = HDIofMCMC(draws_linear[,'X'])
  intercept = HDIofMCMC(draws_linear[,'(Intercept)'])
  linear_parms <- rbind('slope' =slope,'intercept'=intercept)
  colnames(linear_parms) <- c('2.5%','97.5%')
  print(linear_parms)
  
  plotPost2(draws_linear[,'X'] , cex.lab = 1.75 , 
           compVal=0 , ROPE= c(-0.0005,0.0005) , cex.main=1.5 ,
           xlab='Slope', main="Posterior Distribution of Slope Parameter" )
  
  cat('\n Summary of Quadratic fit: \n')
  print(summary(stan_quadratic))
  cat('\n 95% Highest density intervals for Quadratic and intercept parameters: \n')
  quadratic = HDIofMCMC(draws_quadratic[,'I(X^2)'])
  intercept = HDIofMCMC(draws_quadratic[,'(Intercept)'])
  quadratic_parms <- rbind('Quadratic' = quadratic,'intercept'=intercept)
  colnames(quadratic_parms) <- c('2.5%','97.5%')
  print(quadratic_parms)
  
  plotPost2(draws_quadratic[,'I(X^2)'] , cex.lab = 1.75 , 
           compVal=0 , ROPE=c(-0.00005,0.00005) , cex.main=1.5 ,
           xlab='Slope', main="Posterior Distribution of Quadratic Parameter" )
  cat('\n')
  cat('WAIC  Comparison (1st Model is Linear and Second is quadratic): \n')
  waic_linear <- waic(stan_linear)
  waic_quadratic <- waic(stan_quadratic)
  print(compare_models(waic_linear,waic_quadratic))
  
  ###########Compare linear and quadratic trends visually#########
  ###Create candidate x axis value repesentign dsitance from threshold
  Theta_df_short <- data.frame(X = as.vector(rep(c(0:(n_bands-1)),each=100)+ replicate(n_bands,runif(100))))
  ##Get posterior predictions
  posteriors1 <- posterior_linpred(stan_linear,newdata = Theta_df_short)
  posteriors2 <- posterior_linpred(stan_quadratic,newdata = Theta_df_short)
  
  posterior_limits1 <- data.frame(Theta_df_short$X,type ='linear', t(apply(posteriors1,2,quantile,c(0.025,0.5,0.975)))) 
  posterior_limits2 <- data.frame(Theta_df_short$X,type='quadratic',t(apply(posteriors2,2,quantile,c(0.025,0.5,0.975))))
  colnames(posterior_limits1) <- colnames(posterior_limits2) <- c('X','type','perc_2.5','median','perc_97.5')
  posterior_limits <- rbind(posterior_limits1,posterior_limits2)
  
  ggplot(posterior_limits,aes(x=X,group=type))+
    geom_point(data=data_for_reg2,aes(x=X,y=Effectiveness),alpha = 0.2)+
    geom_ribbon(aes(x = X ,ymin = perc_2.5,ymax =perc_97.5 ),fill='grey70')+
    geom_line(aes(x = X ,y = median,col=type))+
    scale_color_manual(values=c('blue','green'), guide=FALSE)+
    ylim(c(0,0.1))+
    facet_grid(type~.)+
    labs(y="",x='Distance from threhsold',title='Linear and Quadratic Fits')
  
}




###Modified plotting function to plot regression parameter estimates#########

plotPost2 = function( paramSampleVec , cenTend=c("mode","median","mean")[1] , 
                     compVal=NULL, ROPE=NULL, credMass=0.95, HDItextPlace=0.7, 
                     xlab=NULL , xlim=NULL , yaxt=NULL , ylab=NULL , 
                     main=NULL , cex=NULL , cex.lab=NULL ,
                     col=NULL , border=NULL , showCurve=FALSE , breaks=NULL , 
                     ... ) {
  # Get min and max of posterior to help plot ROPE
  mini = min(paramSampleVec)
  maxi = max(paramSampleVec)
  
  #Lower end upper end ofadjusted ROPE to be plotted 
  if (!is.null(ROPE)){
    ROPE_l = max(mini,ROPE[1])
    ROPE_u = min(maxi,ROPE[2])
  }
  
  # Override defaults of hist function, if not specified by user:
  # (additional arguments "..." are passed to the hist function)
  if ( is.null(xlab) ) xlab="Param. Val."
  if ( is.null(cex.lab) ) cex.lab=1.5
  if ( is.null(cex) ) cex=1.4
  if ( is.null(xlim) ) xlim=range( c( compVal , ROPE_l,ROPE_u , paramSampleVec ) )
  if ( is.null(main) ) main=""
  if ( is.null(yaxt) ) yaxt="n"
  if ( is.null(ylab) ) ylab=""
  if ( is.null(col) ) col="skyblue"
  if ( is.null(border) ) border="white"
  
  # convert coda object to matrix:
  if ( class(paramSampleVec) == "mcmc.list" ) {
    paramSampleVec = as.matrix(paramSampleVec)
  }
  
  summaryColNames = c("ESS","mean","median","mode",
                      "hdiMass","hdiLow","hdiHigh",
                      "compVal","pGtCompVal",
                      "ROPElow","ROPEhigh","pLtROPE","pInROPE","pGtROPE")
  postSummary = matrix( NA , nrow=1 , ncol=length(summaryColNames) , 
                        dimnames=list( c( xlab ) , summaryColNames ) )
  
  # require(coda) # for effectiveSize function
  postSummary[,"ESS"] = effectiveSize(paramSampleVec)
  
  postSummary[,"mean"] = mean(paramSampleVec)
  postSummary[,"median"] = median(paramSampleVec)
  mcmcDensity = density(paramSampleVec)
  postSummary[,"mode"] = mcmcDensity$x[which.max(mcmcDensity$y)]
  
  HDI = HDIofMCMC(paramSampleVec , credMass )
  postSummary[,"hdiMass"]=credMass
  postSummary[,"hdiLow"]=HDI[1]
  postSummary[,"hdiHigh"]=HDI[2]
  
  # Plot histogram.
  cvCol = "darkgreen"
  ropeCol = "darkred"
  if ( is.null(breaks) ) {
    if ( max(paramSampleVec) > min(paramSampleVec) ) {
      breaks = c( seq( from=min(paramSampleVec) , to=max(paramSampleVec) ,
                       by=(HDI[2]-HDI[1])/18 ) , max(paramSampleVec) )
    } else {
      breaks=c(min(paramSampleVec)-1.0E-6,max(paramSampleVec)+1.0E-6)
      border="skyblue"
    }
  }
  if ( !showCurve ) {
    par(xpd=NA)
    histinfo = hist( paramSampleVec , xlab=xlab , yaxt=yaxt , ylab=ylab ,
                     freq=F , border=border , col=col ,
                     xlim=xlim , main=main , cex=cex , cex.lab=cex.lab ,
                     breaks=breaks , ... )
  }
  if ( showCurve ) {
    par(xpd=NA)
    histinfo = hist( paramSampleVec , plot=F )
    densCurve = density( paramSampleVec , adjust=2 )
    plot( densCurve$x , densCurve$y , type="l" , lwd=5 , col=col , bty="n" ,
          xlim=xlim , xlab=xlab , yaxt=yaxt , ylab=ylab ,
          main=main , cex=cex , cex.lab=cex.lab , ... )
  }
  cenTendHt = 0.9*max(histinfo$density)
  cvHt = 0.7*max(histinfo$density)
  ROPEtextHt = 0.55*max(histinfo$density)
  
  # Display central tendency:
  mn = mean(paramSampleVec)
  med = median(paramSampleVec)
  mcmcDensity = density(paramSampleVec)
  mo = mcmcDensity$x[which.max(mcmcDensity$y)]
  if ( cenTend=="mode" ){ 
    text( mo , cenTendHt ,
          bquote(mode==.(signif(mo,3))) , adj=c(.5,0) , cex=cex )
  }
  if ( cenTend=="median" ){ 
    text( med , cenTendHt ,
          bquote(median==.(signif(med,3))) , adj=c(.5,0) , cex=cex , col=cvCol )
  }
  if ( cenTend=="mean" ){ 
    text( mn , cenTendHt ,
          bquote(mean==.(signif(mn,3))) , adj=c(.5,0) , cex=cex )
  }
  # Display the comparison value.
  if ( !is.null( compVal ) ) {
    pGtCompVal = sum( paramSampleVec > compVal ) / length( paramSampleVec ) 
    pLtCompVal = 1 - pGtCompVal
    lines( c(compVal,compVal) , c(0.96*cvHt,0) , 
           lty="dotted" , lwd=2 , col=cvCol )
    text( compVal , cvHt ,
          bquote( .(round(100*pLtCompVal,1)) * "% < " *
                    .(signif(compVal,3)) * " < " * 
                    .(round(100*pGtCompVal,1)) * "%" ) ,
          adj=c(pLtCompVal,0) , cex=0.8*cex , col=cvCol )
    postSummary[,"compVal"] = compVal
    postSummary[,"pGtCompVal"] = pGtCompVal
  }
  # Display the ROPE.
  if ( !is.null( ROPE ) ) {
    pInROPE = ( sum( paramSampleVec > ROPE[1] & paramSampleVec < ROPE[2] )
                / length( paramSampleVec ) )
    pGtROPE = ( sum( paramSampleVec >= ROPE[2] ) / length( paramSampleVec ) )
    pLtROPE = ( sum( paramSampleVec <= ROPE[1] ) / length( paramSampleVec ) )
    
    
    lines( c(ROPE_l,ROPE_l) , c(0.96*ROPEtextHt,0) , lty="dotted" , lwd=2 ,
           col=ropeCol )
    lines( c(ROPE_u,ROPE_u) , c(0.96*ROPEtextHt,0) , lty="dotted" , lwd=2 ,
           col=ropeCol)
    text( mean(c(ROPE_l,ROPE_u)) , ROPEtextHt ,
          bquote( .(round(100*pLtROPE,1)) * "% < " * .(ROPE[1]) * " < " * 
                    .(round(100*pInROPE,1)) * "% < " * .(ROPE[2]) * " < " * 
                    .(round(100*pGtROPE,1)) * "%" ) ,
          adj=c(pLtROPE+.5*pInROPE,0) , cex=1 , col=ropeCol )
    
    postSummary[,"ROPElow"]=ROPE[1] 
    postSummary[,"ROPEhigh"]=ROPE[2] 
    postSummary[,"pLtROPE"]=pLtROPE
    postSummary[,"pInROPE"]=pInROPE
    postSummary[,"pGtROPE"]=pGtROPE
  }
  # Display the HDI.
  lines( HDI , c(0,0) , lwd=4 , lend=1 )
  text( mean(HDI) , 0 , bquote(.(100*credMass) * "% HDI" ) ,
        adj=c(.5,-1.7) , cex=cex )
  text( HDI[1] , 0 , bquote(.(signif(HDI[1],3))) ,
        adj=c(HDItextPlace,-0.5) , cex=cex )
  text( HDI[2] , 0 , bquote(.(signif(HDI[2],3))) ,
        adj=c(1.0-HDItextPlace,-0.5) , cex=cex )
  par(xpd=F)
  #
  return( postSummary )
}