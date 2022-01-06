 

library(actuar)
	
Loglik <- function(obs, p1, p2, dfun)
{
	p1 <- rep(p1, length.out=length(obs))
	p2 <- rep(p2, length.out=length(obs))
	sum(log(dfun(obs, p1, p2)))
}
aic <- function(obs, p1, p2, dfun, nbpar)
	2*nbpar - 2* Loglik(obs, p1, p2, dfun)
bic <- function(obs, p1, p2, dfun, nbpar)
  log(length(obs))*nbpar - 2* Loglik(obs, p1, p2, dfun)



AICloglikglm <- function(obs, predmean, dispersion, dist, xmin, metric="AIC", object)
{
  if(missing(object))
    stop("missing object argument")
  if(class(object)[1] %in% c("glm", "lm"))
	  nbpar <- length(coef(object))
  else
    nbpar <- length(object)
	if(dist == "lnorm")
	{
		mu <- predmean
		sd <- sqrt(dispersion) #dispersion is sigma^2
		if(metric=="AIC")
		  return(aic(obs, mu, sd, dlnorm, nbpar))
		else if(metric=="BIC")
		  return(bic(obs, mu, sd, dlnorm, nbpar))
		else if(metric=="logLik")
		  return(Loglik(obs, mu, sd, dlnorm))
		
	}
	if(dist == "gamma")
	{
		shape <- 1/dispersion
		rate <- shape/predmean
		if(metric=="AIC")
		  return(aic(obs, shape, rate, dgamma, nbpar))
		else if(metric=="BIC")
		  return(bic(obs, shape, rate, dgamma, nbpar))
		else if(metric=="logLik")
		  return(Loglik(obs, shape, rate, dgamma))
	}
	if(dist == "gaussian")
	{
		mu <- predmean
		sd <- sqrt(dispersion) #dispersion is sigma^2
		if(metric=="AIC")
		  return(aic(obs, mu, sd, dnorm, nbpar))
		else if(metric=="BIC")
		  return(bic(obs, mu, sd, dnorm, nbpar))
		else if(metric=="logLik")
		  return(Loglik(obs, mu, sd, dnorm))
	}
	if(dist == "lgamma")
	{
		shape <- 1/dispersion
		rate <- shape/predmean
		if(metric=="AIC")
		  return(aic(obs, shape, rate, dlgamma, nbpar))
		else if(metric=="BIC")
		  return(bic(obs, shape, rate, dlgamma, nbpar))
		else if(metric=="logLik")
		  return(Loglik(obs, shape, rate, dlgamma))
	}
	if(dist == "pareto1")
	{
		shape <- 1/predmean
		if(metric=="AIC")
		  return(aic(obs, shape, xmin, dpareto1, nbpar))
		else if(metric=="BIC")
		  return(bic(obs, shape, xmin, dpareto1, nbpar))
		else if(metric=="logLik")
		  return(Loglik(obs, shape, xmin, dpareto1))
	}
	NA
}	


	
	

