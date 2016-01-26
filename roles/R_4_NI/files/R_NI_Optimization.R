# Clear objects from memory
rm(list=ls())

noOfAdditionalPhysicians <- 450

colNames = c("tenant_id", "iteration_id", "phy_cmp_1", "phy_prctce_scr_sum", "phy_prctce_uid_cnt", "phy_optmz_init_value", "phy_optmz_upr_bndry", "phy_optmz_lwr_bndry")
practiceData <- read.table(text = "", col.names = colNames)

#=================================================================================================
# Function:     runOptimizations
#
# Arguments:	tenant_id
#				iteration_id
#				no_of_additional_physicians
#				success_callback_url
#				failure_callback_url
#
# Description:  The main function that receives requests and handles flow of this script
#=================================================================================================
runOptimizations <- function(dbIP, dbName, dbUser, dbPass, tenant_id, iteration_id, no_of_additional_physicians, success_callback_url, failure_callback_url) {

	if (!is.numeric(tenant_id)) stop("Tenant id should be a numeric")
	
	if (!is.numeric(iteration_id)) stop("Iteration id should be numeric")
	
	if (!is.numeric(no_of_additional_physicians)) stop("No of additional physicians should be numeric")
	
	if (!is.character(success_callback_url)) stop("Success callback url should be a string")
	
	if (!is.character(failure_callback_url)) stop("Failure callback url should be a string")
	
	# 1) Prepare infrastructure
	
	# Clear the console
	# this does not work in windows
	if ( .Platform$OS.type == "unix" ) cat("\014")
	
	if ( .Platform$OS.type == "windows" ) Sys.setenv("JAVA_HOME"="")
	
	# install packages
	# install.packages("RJDBC", dependencies=TRUE)
	# install.packages("RCurl", dependencies=TRUE)
	# install.packages("stringr", dependencies=TRUE)
	# install.packages("nloptr", dependencies=TRUE)
	# install.packages("gsubfn", dependencies=TRUE)
		
	# Load packages
	library(RJDBC)
	library(RCurl)
	library(stringr)
	library(nloptr)
	library(gsubfn)
	
	driver <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", "sqljdbc4.jar")
	
	queryString <- paste("jdbc:sqlserver://", dbIP, ";databaseName=", dbName)
	
	connection <- dbConnect(driver, queryString, dbUser, dbPass)
	
	# 2) Create log entry and update iteration status
	success <- FALSE
	if ( createLog(tenant_id, iteration_id, "Optimization_Engine_Request_Received", connection) ) {
		success <- updateIterationStatus(tenant_id, iteration_id, connection)
	}

	if (success) {
		assign("noOfAdditionalPhysicians", no_of_additional_physicians, envir = .GlobalEnv)
		
		tryCatch({
			createLog(tenant_id, iteration_id, "Optimization_Engine_Start", connection)
		
			# 3) Prepare modelling data set based on tenant_id and iteration_id
			prepareModellingDataSet(tenant_id, iteration_id, connection)
			print("Modelling Data set prepared")
			
			# 4) Get practice data for tenant
			assign("practiceData", getPracticeData(tenant_id, iteration_id, connection), envir = .GlobalEnv)
			print("Practice data loaded")
			createLog(tenant_id, iteration_id, "Optimization_Engine_Variables_Load_Success", connection)
			
			# current time
			now <- as.character(Sys.Date())
			now <- gsubfn(pattern = "[[:punct:]]", engine = "R", replacement = function(x) ifelse(x == "'", "'", ""),  now)
			print(paste("Current time calculated as ", now, sep = ""))
			
			# 5) Do the optimization
			constraintFunctionFileName <- paste("R_NI_ConstrFn_", tenant_id, iteration_id, now, ".R", sep = "")
			objectiveFunctionFileName <- paste("R_NI_ObjFn_", tenant_id, iteration_id, now, ".R", sep = "")
			doOptimizations(objectiveFunctionFileName, constraintFunctionFileName, tenant_id, iteration_id, connection)
			print("Done optimizing")
			
			# 6) Decompose results
			decomposeResults(tenant_id, iteration_id, connection)
			print("Decomposed results")

			# 7) Delete the scripts created
			if (file.exists(constraintFunctionFileName)) file.remove(constraintFunctionFileName)
			if (file.exists(objectiveFunctionFileName)) file.remove(objectiveFunctionFileName)
			print("deleted scripts")
			
			createLog(tenant_id, iteration_id, "Optimization_Engine_Success", connection)
		}, error = function(e) {
			print(e)
			createLog(tenant_id, iteration_id, "Optimization_Engine_Failed", connection)
			success <- FALSE
		})
	}
	
	dbDisconnect(connection)

	print("posting to ")
	if (success) {
		print(success_callback_url)
		postForm(success_callback_url, style='POST')
	} else {
		print(failure_callback_url)
		postForm(failure_callback_url, style='POST')
	}	
}

#=================================================================================================
# Function:     createLog
#
# Arguments:	tenant_id
#				iteration_id
#				status
#				connection
#
# Description:  This function insert a record in the optimization log table of the portal
#=================================================================================================
createLog <- function(tenant_id, iteration_id, status, connection) {
	insert_query <- paste("INSERT INTO ni_optmz_log(tenant_id, iteration_id, optmz_source, optmz_status) VALUES (", tenant_id, ", ", iteration_id, ", 'Optimization_Engine', '", status, "')", sep = "")
	tryCatch({
		dbSendUpdate(connection, insert_query)
		return(TRUE)
	}, error = function(e) {
		print(e)
		return(FALSE)
	})
}

#=================================================================================================
# Function:     updateIterationStatus
#
# Arguments:	tenant_id
#				iteration_id
#				connection
#
# Description:  This function updates the status of the iteration
#=================================================================================================
updateIterationStatus <- function(tenant_id, iteration_id, connection) {
	update_query <- paste("UPDATE ni_optmz_iteration SET optmz_status = 'Optimization_Engine_Request_Received' WHERE tenant_id = ", tenant_id, " AND iteration_id = ", iteration_id, sep = "")
	tryCatch({
		dbSendUpdate(connection, update_query)
		return(TRUE)
	}, error = function(e) {
		print(e)
		return(FALSE)
	})
}

#======================================================================
# Function:     stripNL
#
# Arguments:	query       The query that needs to be stripped of new line characters
#
# Description:  This function strips New Line characters from a query
#               This allows us to write multi-line queries, and when
#               they are submitted to the database, they are clean and
#               database ready
#======================================================================
stripNL <- function(query) {
	query <- str_replace_all(query, '\n', '')
}

#======================================================================
# Function:     prepareModellingDataSet
#
# Arguments:	tenant_id       The tenant whose data needs to be processed
#   			iteration_id    A unique iteration id
#				connection
#
# Description:  This function helps with preparing the modelling data set in SQL Server
#======================================================================
prepareModellingDataSet <- function(tenant_id, iteration_id, connection) {
	prepare_query <-paste(
          " declare @tenant_id    int;
            set @tenant_id = ",
          tenant_id,";
            declare @iteration_id int;
            set @iteration_id = ",
          iteration_id,";
          exec ni_sp_calc_phy_optmz_vars @tenant_id = ",
          tenant_id,", 
          @iteration_id = ",
          iteration_id,";
          exec ni_sp_calc_prctce_optmz_vars @tenant_id = ",
          tenant_id,",
          @iteration_id = ",
          iteration_id,";",
          sep = ""
        )
	prepare_query <- stripNL(prepare_query)
	dbSendUpdate(connection, prepare_query)
}

#======================================================================
# Function:     getPracticeData
#
# Arguments:	tenant_id         The tenant whose data needs to be processed
#   			iteration_id      A unique iteration id
#				connection
#
# Description:  This function helps with getting the data for a tenant id
#======================================================================
getPracticeData <- function(tenant_id, iteration_id, connection) {
	get_query <-paste(
          " declare @tenant_id    int;
            set @tenant_id = ",
          tenant_id,";
            declare @iteration_id int;
            set @iteration_id = ",
          iteration_id,";
          exec ni_sp_calc_prctce_optmz_vars @tenant_id = ",
          tenant_id,", 
          @iteration_id = ",
          iteration_id,";",
          sep = ""
        )
	get_query <- stripNL(get_query)
	return(data.frame(dbGetQuery(connection, get_query)))
}

#======================================================================
# Function:     doOptimizations
#
# Arguments:	objectiveFunctionFileName
#				constraintFunctionFileName
#				tenant_id
#				iteration_id
#				connection
#
# Description:  The wrapper function to do the optimization
#======================================================================
doOptimizations <- function(objectiveFunctionFileName, constraintFunctionFileName, tenant_id, iteration_id, connection) {
 
	# 1) Create Objective Function using Practice Data
	createObjectiveFunction(objectiveFunctionFileName)
	print("Objective function created")
	
	# 2) Create Constraint Function using Practice Data
	createConstraintFunction(constraintFunctionFileName)
	print("Constraint function created")
	
	# 3) Initialize variables
	x0 <- as.numeric(practiceData$phy_optmz_init_value) #assignInitialValues
	lb <- as.numeric(practiceData$phy_optmz_lwr_bndry) #assignLowerBound
	ub <- as.numeric(practiceData$phy_optmz_upr_bndry) #assignUpperBound
	
	# 4) Pick Algorithm to be used
	local_opts <- list (
		"algorithm"   = "NLOPT_LD_MMA",
		"xtol_rel"    = 1.0e-7
	)
	opts <- list (
		"algorithm"   = "NLOPT_LD_AUGLAG",
		"xtol_rel"    = 1.0e-7,
		"maxeval"     = 1000,
		"local_opts"  = local_opts 
	)

	# 5) Do optimization
	optRes <- nloptr (
		x0          = x0,
		eval_f      = eval_f,
		lb          = lb,
		ub          = ub,
		eval_g_ineq = eval_g_ineq,
		opts        = opts
	)

	# 6) Process Results
	optSol                 			<- data.frame(optRes$solution)             	# a) Get Decision Variables (DV)
	names(optSol)          			<- c("DV")                                 	# b) Name DVs
	optSol$result					<- ifelse(optSol$DV>0, 1, optSol$DV)       	# c) Round DVs
	practiceData$phy_prctce_sel	<- optSol$result                           		# d) Merge DVs
	
	print("Deleting previous results")
	deletePreviousResults(tenant_id, iteration_id, connection)        			# e) Delete previous results 
	
	print("Inserting new results")
	apply(practiceData, 1, insertNewResults, connection)						# f) Save results
}

#======================================================================
# Function:     createObjectiveFunction
#
# Arugments:    objectiveFunctionFileName
#
# Description:  Helper to create and load Objective Function at run
#               time
#======================================================================
createObjectiveFunction <- function(objectiveFunctionFileName) {
	# 1) Get objective function coefficients
	objectiveEquation  <- createObjectiveEquation() 

	# 2) Create objective function at run time
	objectiveFunction  <- paste(
		"eval_f <- function(x) {
			return( 
				list(
					\"objective\" = -",objectiveEquation,",
					\"gradient\"  = -1*(practiceData$phy_prctce_scr_sum)
				)
			)
		}"
	)

	# 3) Write objective function to a file
	write(
		objectiveFunction, 
		file    = objectiveFunctionFileName, 
		append  = FALSE, 
		sep     = " "
	)

	# 4) Load objective function into memory
	source(objectiveFunctionFileName)
}

#======================================================================
# Function:     createObjectiveEquation
#
# Description:  Helper to create Objective Equation
#======================================================================
createObjectiveEquation <- function() {
	paste(
		paste("(",
			practiceData$phy_prctce_scr_sum,
			"*",
			"x[",
			1:nrow(practiceData),
			"])\n",
			sep=""
		),
		collapse=" - "
	)
}

#======================================================================
# Function:     createConstraintFunction
#
# Arguments:    constraintFunctionFileName
#
# Description:  Helper to create Constraints Function
#======================================================================
createConstraintFunction <- function(constraintFunctionFileName) {
	# Step 1: Create constraint equation function
	constraintEquation <- createConstraintEquation()

	# Step 2: Create constraint function
	constraintFunction <- paste (
		"eval_g_ineq <- function(x) {
			constr <- c(", constraintEquation, ")
			grad   <- c(practiceData$phy_prctce_uid_cnt)
			return(
				list(
					\"constraints\" = constr,
					\"jacobian\"    = grad
				)
			)
		}"
	)

	# Step 3: Write constraint function to a file
	write(
		constraintFunction, 
		file    = constraintFunctionFileName, 
		append  = FALSE, 
		sep     = " "
	)

	# Step 4: Load Constraint function into memory
	source(constraintFunctionFileName)
}

#======================================================================
# Function:     createConstraintEquation
#
# Description:  Helper to create Constraints Equation
#======================================================================
createConstraintEquation <- function() {
	paste(
		paste(
			paste( 
				practiceData$phy_prctce_uid_cnt,
				"*",
				"x[",
				1:nrow(practiceData),
				"]\n",
				sep=""
			),
			collapse=" + "
		),
		" - noOfAdditionalPhysicians",
		sep=""
	)
}

#=========================================================================
# Function:     deletePreviousResults
#
# Arguments:	tenant_id         The tenant whose data needs to be processed
#   			iteration_id      A unique iteration id
#   			connection
#  
# Description:  Helps to delete previous results
#=========================================================================
deletePreviousResults <- function(tenant_id, iteration_id, connection) {
	update_query <- paste("delete from ni_optmz_prctce_result where tenant_id = ", tenant_id, " and iteration_id = ", iteration_id, sep = "")
	dbSendUpdate(connection, update_query)
}

#=========================================================================
# Function:     insertNewResults
#
# Arguments:	record
#   			connection
#  
# Description:  Helps to insert new results
#=========================================================================
insertNewResults <- function(record, connection) {
	record[3] <- str_replace_all(record[3], "'", "''")
	insert_query <- "INSERT INTO ni_optmz_prctce_result (tenant_id, iteration_id, phy_cmp_1, phy_prctce_scr_sum, phy_prctce_uid_cnt, phy_optmz_init_value, phy_optmz_upr_bndry, phy_optmz_lwr_bndry, phy_prctce_sel) VALUES ("
	insert_query <- paste(insert_query, record[1], ", ", record[2], ", '", record[3], "'", sep = "")
	insert_query <- paste(insert_query, record[4], record[5], record[6], record[7], record[8], record[9], sep = ", ")
	insert_query <- paste(insert_query, ")", sep = "")
	dbSendUpdate(connection, insert_query)
}

#=========================================================================
# Function:     decomposeResults
#
# Arguments:	tenant_id         The tenant whose data needs to be processed
#   			iteration_id      A unique iteration id
#				connection
#   
# Description:  Helps to decompose optimization results at physician level
#=========================================================================
decomposeResults <- function(tenant_id, iteration_id, connection) {
	decompose_query <- paste(
			"declare @tenant_id    int;
            set @tenant_id = ",
			tenant_id,";
            declare @iteration_id int;
            set @iteration_id = ",
			iteration_id,";
            exec ni_sp_decomp_prctce_optmz_result @tenant_id =",
			tenant_id,",
            @iteration_id = ",
			iteration_id,";",
			sep = ""
        )
	decompose_query <- stripNL(decompose_query)
	dbSendUpdate(connection, decompose_query)
}