# set working directory ####
getwd()
#setwd("/User")

# load data into R ####
data <- read.csv("path.csv")

# load libraries ####
require(lavaan)
require(semPlot)

View(data)

# Direct effects ####
# with no mediation variable at all

grad <- (data$grad)
hs <- (data$hs)
gre <- (data$gre)
col <- (data$col)

# 1. Direct effect of College GPA on Grad GPA (The "c" path)
model_direct_col <- 'grad ~ col'
fit_direct_col <- sem(model = model_direct_col, data = data)

print("--- Direct Effect of College GPA on Grad GPA ---")
summary(fit_direct_col, fit.measures=TRUE, standardized=TRUE)


# 2. Direct effect of High School GPA on Grad GPA
model_direct_hs <- 'grad ~ hs'
fit_direct_hs <- sem(model = model_direct_hs, data = data)

print("--- Direct Effect of High School GPA on Grad GPA ---")
summary(fit_direct_hs, fit.measures=TRUE, standardized=TRUE)



model_mediation <- '
  # 1. Direct effect of IV on DV (c_prime path)
  grad ~ c_prime * col
  
  # 2. Effect of IV on Mediator (a path)
  gre ~ a * col
  
  # 3. Effect of Mediator on DV (b path)
  grad ~ b * gre
  
  # 4. Calculate Indirect and Total Effects
  indirect_effect := a * b
  total_effect := c_prime + (a * b)
'

fit_mediation <- sem(model = model_mediation, data = data)
summary(fit_mediation, fit.measures=TRUE, standardized=TRUE)

# Yes, your mediation model worked, and every single path is statistically significant. 
#The results show that GRE scores act as a significant "middleman" (mediator) 
#between College GPA and Grad GPA, but they don't explain the whole story (partial mediation).
# Indirect effects ####
model2 <- '
  # Direct effect (c_prime path)
  grad ~ c_prime * col
  
  # Effect on Mediator (a path)
  gre ~ a * col
  
  # Effect of Mediator (b path)
  grad ~ b * gre
  
  # Calculate Indirect and Total Effects
  indirect_effect := a * b
  total_effect := c_prime + (a * b)
'

# 1. Fit the standard model 
fit <- sem(model = model2, data = data)

# 2. Fit the model again using bootstrapping
# We set bootstrap = 1000, which is a standard number of resamples.
fit_boot <- sem(model = model2, data = data, se = "bootstrap", verbose = TRUE, bootstrap = 1000)

# 3. View the summary with the bootstrapped standard errors
summary(fit_boot, fit.measures = TRUE, standardized = TRUE)



# Visualisation ####
semPaths(fit ,title=TRUE, curvePivot = TRUE, what="std", style="Openmx", layout="tree2", thresholds="FALSE",sizeMan=7, nCharNodes=1, col="darkblue", fixedStyle = c("purple",1), freeStyle = c("blue",1), edge.label.cex=0.7, edge.width=0.8)


#What is the direct effect of col on grad before controlling for mediation?
XXXX
#What is the direct effect of col on grad after controlling for mediation?
XXXX
#What is the indirect effect of col on grad (via GRE)?
XXXX
#Does the indirect effect (partial or fully mediates) or (does not mediate) the relationship between COL or GRAD?
XXXX







#Notes the may be helpful 
# what determines the types of values going into the parameters (std, est, eq )
# The style to use (lisrel, ram, mx, Openmx, default = ram)
# layout (tree, circle, spring, tree2, circle2)
# sizeMan (size of manifest variables width default = 5)
# sizeMan2 (height of manifest variables, default = follows sizeMan)
# shapeLat (ellipse, triangle. default = cirle)
# sizeLat (size of latent variables width, default = 8 )
# sizeLat2 (size of latent variables height, default = follows sizeLat)
# nCharNodes (Number of characters to abbreviate node labels to)
# fixedStyle = c("red",1). fixed parameters will be red in colour. 1 = solid lines, 2 = dashed lines etc.
# freeStyle = c("blue",4). free parameters will be blue in color. 1 = solid lines, 2 = dashed lines etc.
# edge.label.cex. default = 0.8. Controls the font size of the edge labels.
# edge.width=0.8. Is the thickness of the arrows
# node.width scales the width of nodes.









