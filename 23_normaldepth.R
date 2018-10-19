# *------------------------------------------------------------------
# | PROGRAM NAME: 23_normaldepth
# | FILE NAME: 23_normaldepth.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  This code calculates normal depth for a trapezoidal channel
# |
# *------------------------------------------------------------------

###########################################################################
## Set the Paths
###########################################################################
### Path for Data and Output	
data_path <- "./data"
output_path <- "./output"

### Set output location
write_output_base_path <- output_path
dir.create(write_output_base_path)

###########################################################################
###  Load Packages
###########################################################################
### Load required packages
require(tidyverse)

###########################################################################
###  Write Functions for Geometry of Trapezoidal Channels
###########################################################################

### Function to calculate the cross-sectional area of a trapezoidal channel
area_trap <- function(y, b, m){
	a <- b*y+m*y^2
	return(a)
}

### Function to calculate wetted perimeter of a trapezoidal channel
wp_trap <- function(y, b, m){
	wp <- b+2*y*(1+m^2)^0.5
	return(wp)
}

### Function to calculate top width of a trapezoidal channel
top_trap <- function(y,b,m){
	top <- b+2*m*y
	return(top)
}

### Function to calculate channel velocity using Manning's equation
manning_vel <- function(R, S, n, units="imp"){
	if (units == "metric"){
		k <- 1
	} else {
		k <- 1.49
	}
	
	vel <- (k/n)*(R)^(2/3)*(S^(1/2))
	return(vel)
}


###########################################################################
###  Example Problem 1 - Solving Graphically
###########################################################################
### Set givens
Q_design_ex1 <- 100
b_ex1 <- 20
m_ex1 <- 2
s_ex1 <- 0.001
n_ex1 <- 0.03

### Create a plot of depth vs flow
### Start by creating a data frame with a sequence of depths from 0 to 3 ft at 0.1 ft increments
results_df <- data.frame(depth=seq(0,3,0.1))

### Create a column for area
results_df$area <- area_trap(y=results_df$depth, b=b_ex1, m=m_ex1)

### Show results 
results_df

### Create columns for wetted perimeter and hydraulic radius
results_df$wp <- wp_trap(y=results_df$depth, b=b_ex1, m=m_ex1)
results_df$hyd_rad <- results_df$area / results_df$wp

### Show results. The head command shows just the first few values of the dataframe
head(results_df)

### Calculate velocity using Manning's Eq
results_df$vel <- manning_vel(R=results_df$hyd_rad, S=s_ex1, n=n_ex1, units="imp")

### Calculate flow by multiplying by area
results_df$flow <- results_df$vel * results_df$area

### Check results. The head command shows just the first few values of the dataframe
head(results_df)
tail(results_df)

### Check results for nearest to 100 cfs. Look at rows 19 to 22
results_df[seq(19,22),]

###########################################################################
###  Plot results for Example Problem 1
###########################################################################
### Set up output folders
write_output_path <- file.path(write_output_base_path, "lecture_23")
dir.create(write_output_path, recursive=TRUE)

### Create Plot, save to p
p <- ggplot(results_df, aes(x=flow, y=depth)) %>%
	+ geom_line() %>%
	+ geom_vline(xintercept = 100, color="blue", linetype="longdash")  %>%
	+ theme_classic()  %>%
	+ scale_x_continuous(name="Flow (cfs)") + scale_y_continuous(name="Depth (ft)")

### Display plot on screen
p	
	
### Save figure
ggsave(file.path(write_output_path, "example1_flow_v_depth.png"), p, width=5, height=4, dpi=600)

### Output dataframe
write.csv(results_df, file.path(write_output_path,"example1_flow_v_depth.csv"), row.names=FALSE)



###########################################################################
###  Example Problem 1 - Solving Exactly 
###########################################################################
### For this problem, we will use R's root solver (uniroot)

### For uniroot to work, we need to define the function
### This function will take in a depth and export an error (flow calculated - flow design)

ex_1_error_func <- function(depth){
	### Calculate area, wetted perimeter and hydraulic radius as before
	area <- area_trap(y=depth, b=20, m=2)
	wp <- wp_trap(y=depth,  b=20, m=2)
	hyd_rad <- area / wp

	### Calculate velocity using Manning's Equation
	### Then calculate flow by multiplying velocityby area
	vel <- manning_vel(R=hyd_rad, S=0.001, n=0.03)
	flow <- vel*area

	### Calculate error as the difference in calculated and design flow
	### Return this value
	error <- flow - 100
	return(error)
}

### Plug this function into uniroot and tell it to search over a reasonable range (0 to 10 ft)
u1 <- uniroot(ex_1_error_func, interval=c(0,10))
### Display the result
u1







