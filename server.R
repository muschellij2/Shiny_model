rm(list=ls())
options(stringsAsFactors=FALSE)
library(xtable)
library(car)

##########################
### Generating Random Data
##########################
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
Bad_Outcome_Day_365[c(1, 3, 5, 7)] <- NA

rando <- data.frame(Age, Enrollment_GCS_Add, Group_Assigned, ICH_Pre_Rand_10, ICH_EOT_10, IVH_EOT_10, Bad_Outcome_Day_180, Bad_Outcome_Day_365)
rando$Under_15cc <- Under_15cc
rando$Group_Assigned <- Group_Assigned

#### just for testing - not used
input <- list(y_var = "Bad_Outcome_Day_180", x_var = c("Enrollment_GCS_Add", "Age", "ICH_Pre_Rand_10", "Group_Assigned", "ICH_EOT_10", "IVH_EOT_10"), fam="quasibinomial()")


shinyServer(function(input, output) {

	dataset <- reactive({
	    if (is.null(input$files)) {
	      # User has not uploaded a file yet
	      df <- rando
	    } else {
	    	print(input$files$datapath)
		    df <- read.csv(input$files$datapath, header=TRUE, stringsAsFactors =TRUE)
	    	
	    }
	    #print("FILES")
	    #print(head(df))
	    #print(input$files)
	   
	})
	
# 	data <- reactive(function(){
# 	  
# 	  path <- input$url
# 	  
# 	  #translate relative paths to server-friendly paths
# 	  if (substr(input$url, 0, 2) == "./"){
# 	    path <- paste("./www/", substring(input$url, 3), sep="")
# 	  }      
# 	  
# 	  
# 	  data <- read.csv(path, row.names=1)
# 	  
# 	  #ensure that each row is a gene.
# 	  if (input$orientation == "Sample"){
# 	    data <- t(data)
# 	  }
# 	  data
# 	})  
	

  # ------------------------------------------------------------------
  # Functions for creating models and printing summaries

	## RUN_MOD takes in  formula - runs a 
	run_mod <- function(formula, fam, df){
		# print(head(df))
		### runs model and makes table 
		## make fam the actual family
		ornames <- c("Odds Ratio", "Relative Risk", "Beta Estimate")
		pick <- grepl("binomial", fam)*1 + grepl("poisson", fam)*2 + grepl("gauss", fam)*3
		estname <- ornames[pick]
		fam <- eval(parse(text=fam))
		mod <- glm(formula=formula, data=df, family=fam)
		s <- summary(mod)
		cos <- coef(s)
    #print(cos)
		torz <- colnames(cos)[grepl("Pr", colnames(cos))]
		torz <- gsub("Pr\\(>\\|([t|z])\\|\\)", "\\1", torz)
		est <- cos[, "Estimate"] 
		nms <- rownames(cos)
		## grab either t or z critical point for CI
		crit_point <- switch(torz,
			"t"= qt(0.975, df=mod$df.residual),
			"z" = qnorm(0.975))
		CI <- matrix(sapply(c(-1,1), function(one) est + one * crit_point*cos[, "Std. Error"]), ncol=2)
		est <- fam$linkinv(est)
		CI <- fam$linkinv(CI)
		CI <- apply(CI, 1, function(x) sprintf("(%4.3f, %4.3f)", x[1], x[2]))
		p.value <- sapply(cos[, paste0("Pr(>|",torz, "|)")], sprintf, fmt="%04.3f")
		est <- sapply(est, sprintf, fmt="%4.3f")
# 		print(est)
# 		print(p.value)
#     print(CI)
#     print(names(est))
#     names(est) <- names(CI)
		mat <- data.frame(as.matrix(cbind(Var=nms, TMP=est, "95% CI"= CI, "P-value"=p.value)))
    cn <- c("Var", estname, "95% CI", "P-value")
#     print(cn)
		colnames(mat) <- cn
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
		mat["N", estname] <- length(mod$y)
		#print(mat$"Odds Ratio")

		xmat <- xtable(mat)
		return(list(mod=mod, xmat=xmat))
	}


	getmod <- reactive({ 
		df <- dataset()
		
		formula <- paste(input$y_var, "~  ", paste0(c("1", input$x_var), collapse="+ "))
		fam <- input$fam
		# print(fam)
		run_mod(formula, fam=fam, df=df)
	})


  output$mod_linear_text <- renderPrint( {
  	tmp <- getmod()
	xmat <- tmp$xmat
	print(xmat, type="html")
  })

	output$Controls <- renderUI({
		df <- dataset()		
		checkboxGroupInput("x_var", "Choose Covariates", colnames(df))
	})  
	output$Outcome <- renderUI({
		df <- dataset()		
		selectInput("y_var", "Choose Outcome", colnames(df))
	})    
	output$main_plot <- renderPlot({
	  	tmp <- getmod()
		mod <- tmp$mod
		print(mod)
	avPlots(mod)
	})
  



})
