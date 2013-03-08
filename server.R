rm(list=ls())
options(stringsAsFactors=FALSE)
library(ggplot2)
library(xtable)
library(car)

set.seed(20130306)
Age <- rnorm(100, mean=55, sd=4)
rr <- runif(15)
rr <- rr/sum(rr)
Enrollment_GCS_Add <- rbinom(100, 15, prob=rr)
grps <- c("Medical", "Surgical")
Group_Assigned <- factor(grps[sample(c(1, 2), 100, replace=TRUE, prob=c(0.5,0.5))], levels=grps)

ICH_Pre_Rand_10 <- runif(100, min=0, max=.5) + rexp(100, rate=5)*10
ICH_EOT_10 <- ICH_Pre_Rand_10 - (2* (Group_Assigned == "Surgical") + 0.14* (Group_Assigned == "Medical")) + rnorm(100)
IVH_EOT_10 <- runif(100, min=0, max=1)
grps <- c("More than 15cc at EOT", "Less or equal to 15cc at EOT")
Under_15cc <- grps[((ICH_EOT_10 < 1.5) + 1)]
Under_15cc <- factor(Under_15cc, levels= grps)
Bad_Outcome_Day_180 <- rbinom(100, 1, prob=0.3)
Bad_Outcome_Day_365 <- rbinom(100, 1, prob=0.3)

rando <- data.frame(Age, Enrollment_GCS_Add, Group_Assigned, ICH_Pre_Rand_10, ICH_EOT_10, IVH_EOT_10, Bad_Outcome_Day_180, Bad_Outcome_Day_365)
rando$Under_15cc <- Under_15cc
rando$Group_Assigned <- Group_Assigned


# Returns a logical vector of which values in `x` are within the min and max
# values of `range`.
in_range <- function(x, range) {
  x >= min(range) & x <= max(range)
}
input <- list(y_var = "Bad_Outcome_Day_180", x_var = c("Enrollment_GCS_Add", "Age", "ICH_Pre_Rand_10", "Group_Assigned", "ICH_EOT_10", "IVH_EOT_10"))


shinyServer(function(input, output) {

	dataset <- function(){
		rando
	}

  # ------------------------------------------------------------------
  # Functions for creating models and printing summaries

	run_mod <- function(formula){
		
		### runs model and makes table 
		mod <- glm(formula=formula, data=rando, family=binomial())
	
		s <- summary(mod)
		cos <- coef(s)
		est <- cos[, "Estimate"] 
		CI <- sapply(c(-1,1), function(one) est + one *qnorm(0.975)*cos[, "Std. Error"])
		est <- exp(est)
		CI <- exp(CI)
		CI <- apply(CI, 1, function(x) sprintf("(%4.3f, %4.3f)", x[1], x[2]))
		p.value <- sapply(cos[, "Pr(>|z|)"], sprintf, fmt="%04.3f")
		est <- sapply(est, sprintf, fmt="%4.3f")
		mat <- data.frame(cbind(Var=names(est), "Odds Ratio"=est, "95% CI"= CI, "P-value"=p.value))
		colnames(mat) <- c("Var", "Odds Ratio", "95% CI", "P-value")
		labels <- c("Intercept" = "(Intercept)", 
				"Enrollment GCS" = "Enrollment_GCS_Add",                      
	    		"Age" = "Age",   
	    		"Pre-Randomization ICH (per 10cc)" = "ICH_Pre_Rand_10",	                   
	    		"Surgery vs. Medical" = "Group_AssignedSurgical", 
				"Less or equal to 15cc at EOT" = "Under_15ccLess or equal to 15cc at EOT",    		
	    		"End of Treatment ICH (per 10cc)" = "ICH_EOT_10", 
	    		"End of Treatment IVH (per 10cc)" = "IVH_EOT_10"
	)
		labels <- data.frame(cbind(Var = labels, Label=names(labels)))
		mat <- merge(mat, labels, sort=FALSE, all.y=TRUE, by="Var")	
		# mat$Label[is.na(mat$Label)] <- mat$Var[is.na(mat$Label)]
		rownames(mat) <- mat$Label
		mat <- mat[labels$Label, ]
		mat$Label <- NULL
		mat$Var <- NULL
		N <- rep("", ncol(mat))
		mat <- rbind(mat, N=N)
		mat["N", "Odds Ratio"] <- length(mod$y)
		#print(mat$"Odds Ratio")

		xmat <- xtable(mat)
		return(list(mod=mod, xmat=xmat))
	}


	getmod <- reactive({ 
		formula <- paste(input$y_var, "~", paste0(input$x_var, collapse="+ "))
		run_mod(formula)
	})


  output$mod_linear_text <- renderPrint( {
  	tmp <- getmod()
	xmat <- tmp$xmat
	print(xmat, type="html")
  })
  
	output$main_plot <- renderPlot({
	  	tmp <- getmod()
		mod <- tmp$mod

	avPlots(mod)
	})
  



})
