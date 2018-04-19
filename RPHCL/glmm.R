# Copyright 2018 Roland Schäfer <mail@rolandschaefer.net>
# Distributed under the following BSD 2-clause license.
#
# BEGIN OF BSD 2-CLAUSE LICENSE
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
# OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
# AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
# WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#
# END OF BSD 2-CLAUSE LICENSE

# The data file "glmm.RData" included with this script is licensed
# under a Creative Commons Attribution 4.0 International License (see
# http://creativecommons.org/licenses/by/4.0/).

# Reference for script and data:
# Roland Schäfer (2018). Abstractions and exemplars: the measure noun phrase alternation in German. To appear in Cognitive Linguistics.
# https://github.com/rsling/measurenps


### BEGIN OF SCRIPT

# If you execute this step by step, first change into the directory
# where the script and data are located.

# If you source it, use the following command:
# source("glmm.R", chdir = T)

# This erases your whole environment. It is good practice to begin
# self-contained scripts with this to avoid reliance on a specific
# state of the environment or on side-effects
rm(list=ls(all=T))

# Seed the random number generator. If you execute this script
# strictly step by step, all numbers will be exactly as in the
# chapter.
set.seed(23846)

# Required packages.
require(lme4)
require(fmsb)
require(MuMIn)
require(car)
require(pbkrtest)
library(merTools)


# Some options.
save.plots <- TRUE


# This loads the data, i.e. the "measure" data frame and copyright
# information.
load("measure.RData")


# First, let's try a GLM with the measure noun lemma as a fixed
# effect.
glm.01 <- glm(Construction~1
              +Measurelemma      # Lemma effect.
              +Badness           # Item-level effects.
              +Cardinal
              +Genitives
              +Measurecase
              , data=measure, family=binomial(link=logit))
glm.01.r2 <- NagelkerkeR2(glm.01)
print(summary(glm.01))
print(glm.01.r2)


# Now let's try a GLMM with the measure noun lemma as a random
# effect and without tuning the optimiser.
glmm.01 <- glmer(Construction~1
                 +(1|Measurelemma)  # Lemma effect.
                 +Badness           # Item-level effects.
                 +Cardinal
                 +Genitives
                 +Measurecase
                 , data=measure, family=binomial(link=logit))
glmm.01.r2 <- r.squaredGLMM(glmm.01)
print(summary(glmm.01))
print(glmm.01.r2)


# If we only want to extract the variance-covariance matrix from the fit.
print(VarCorr(glmm.01))


# Generate confidence intervals for random effect variance estimate.
glmm.01.varconf <- confint(glmm.01, parm="theta_", method="profile")
print(glmm.01.varconf)

# (Not always) more robust method using bootstrap.
# TAKES A LONG TIME TO COMPUTE! Uncomment if desired.
# glmm.01.varconf.boot <- confint(glmm.01, parm="theta_", method="boot", nsim = 250)
# print(glmm.01.varconf.boot)


# Compare the GLM R2 and the GLMM R2.
cat("GLM Nagelkerke R2 =", glm.01.r2$R2)
cat("GLMM marginal R2 =",glmm.01.r2[[1]])
cat("GLMM conditional R2 =",glmm.01.r2[[2]])


# Get prediction intervals for Measurelemma.
glmm.01.predict <- predictInterval(glmm.01, n.sims = 100)


# Plots the intercept from model for the number most frequent levels in data.
ranef.plot <- function(model, effect, number = -1, intervals = TRUE, ...) {
  require(lme4)

  # Get conditional modes.
  .ranef <- ranef(model, condVar = TRUE, drop = T)[[effect]]

  # Get a subsample of conditionalk modes if necessary.
  if (number > 0 & length(.ranef) > number)
    .select <- sample(1:length(.ranef), number)
  else
    .select <- 1:length(.ranef)

  # Order indices of the selection by conditional modes.
  .select <- .select[order(.ranef[.select])]

  # Create full data frame.
  .condvar <- attributes(.ranef)$postVar
  .df <- cbind(.ranef[.select],
               .ranef[.select]-.condvar[.select],
               .ranef[.select]+.condvar[.select])

  # Plot.
  dotchart(.df[,1], labels = rownames(.df), xlim = c(min(.df[,2]), max(.df[,3])), ...)

  # Add the prediction intervals.
  if (intervals) for (.i in 1:nrow(.df)) lines(c(.df[.i,2], .df[.i,3]), c(.i,.i), lwd=2)

  # Return the data frame with the selection.
  .df
}
opts.dotchart <- list(pch=19, col="black", cex=1, xlab="Prediction of conditional mode")

if(save.plots) pdf("ranef_selection.pdf")
do.call(ranef.plot, c(list(glmm.01, "Measurelemma", 30, main = "Measure lemma random effect"), opts.dotchart))
if(save.plots) dev.off()



# Now let's try a GLMM with the measure noun lemma as a random
# effect.
glmm.02 <- glmer(Construction~1
                 +(1|Measurelemma)  # Lemma effect.
                 +Badness           # Item-level effects.
                 +Cardinal
                 +Genitives
                 +(1|Measurecase)
                 , data=measure, family=binomial(link=logit))
glmm.02.r2 <- r.squaredGLMM(glmm.02)
print(summary(glmm.02))
print(glmm.02.r2)

# Generate confidence intervals for random effect variance estimate.
glmm.02.varconf <- confint(glmm.02, parm="theta_", method="profile")
print(glmm.02.varconf)


# Full model as used in the paper.
# Notice that it is difficult to put the random number generator in
# exactly the same state as it was in the script used for the paper.
# This is why number differ from the journal paper. However, they are
# exactly the same as in the PHCL chapter.
glmm.03 <- glmer(Construction~1
                 +(1|Measurelemma)
                 +(1|Kindlemma)
                 +Badness
                 +Cardinal
                 +Genitives
                 +Measurecase
                 +Kindattraction
                 +Kindfreq
                 +Kindgender
                 +Measureattraction
                 +Measureclass
                 +Measurefreq
                 , data=measure, family=binomial(link=logit), na.action = na.fail,
                 control=glmerControl("bobyqa"))
glmm.03.r2 <- r.squaredGLMM(glmm.03)
print(summary(glmm.03))
print(glmm.03.r2)


# We now calculate a prediction for item 99 step by step.
data.99 <- measure[99,]                              # The measured data.
glmm.03.ranef <- ranef(glmm.03)                      # Conditional modes.
glmm.03.fixef <- coef(summary(glmm.03))[,'Estimate'] # Fixed effects.

# Factors are complicated because of the way estimates are
# presented in the model summary. This just creates a factor level
# name to be used later.
data.99.Measureclass <- paste0('Measureclass', unlist(lapply(data.99$Measureclass, as.character)))
data.99.Kindgender   <- paste0('Kindgender', unlist(lapply(data.99$Kindgender, as.character)))
data.99.Cardinal     <- paste0('Cardinal', unlist(lapply(data.99$Cardinal, as.character)))
data.99.Measurecase  <- paste0('Measurecase', unlist(lapply(data.99$Measurecase, as.character)))

# The value from the Measurelemma second-level model.
data.99.measurelevel <- unlist(unname(
  glmm.03.ranef$Measurelemma[as.character(data.99$Measurelemma),'(Intercept)'] +
    glmm.03.fixef['Measureattraction'] * data.99['Measureattraction']          +
    glmm.03.fixef['Measurefreq']       * data.99['Measurefreq']                +
    ifelse(data.99.Measureclass %in% names(glmm.03.fixef), glmm.03.fixef[data.99.Measureclass], 0)
))

# The value from the Kindlemma second-level model.
data.99.kindlevel <- unlist(unname(
  glmm.03.ranef$Kindlemma[as.character(data.99$Kindlemma),'(Intercept)']   +
    glmm.03.fixef['Kindattraction'] * data.99['Kindattraction']            +
    glmm.03.fixef['Kindfreq']       * data.99['Kindfreq']                  +
    ifelse(data.99.Kindgender %in% names(glmm.03.fixef), glmm.03.fixef[data.99.Kindgender], 0)
))

# Full model prediction.
data.99.predict.by.hand <- unlist(unname(
  glmm.03.fixef['(Intercept)']                                                             +
    data.99.measurelevel                                                                   +
    data.99.kindlevel                                                                      +
    glmm.03.fixef['Badness']   * data.99['Badness']                                        +
    glmm.03.fixef['Genitives'] * data.99['Genitives']                                      +
    ifelse(data.99.Cardinal %in% names(glmm.03.fixef), glmm.03.fixef[data.99.Cardinal], 0) +
    ifelse(data.99.Measurecase %in% names(glmm.03.fixef), glmm.03.fixef[data.99.Measurecase], 0)
))


# Compare with the built-in prediction function.
data.99.predict <- predict(glmm.03)[99]

print(invlogit(data.99.predict.by.hand))
print(invlogit(data.99.predict))
coef(summary(glmm.03))['Kindfreq', 'Estimate'] *
  measure[99, 'Kindfreq']


# The same model with Measureclass as a nesting random effect.
glmm.04 <- glmer(Construction~1
                 +(1|Measurelemma)
                 +(1|Kindlemma)
                 +Badness
                 +Cardinal
                 +Genitives
                 +Measurecase
                 +Kindattraction
                 +Kindfreq
                 +Kindgender
                 +Measureattraction
                 +(1|Measureclass)
                 +Measurefreq
                 , data=measure, family=binomial(link=logit), na.action = na.fail,
                 control=glmerControl("bobyqa"))
glmm.04.r2 <- r.squaredGLMM(glmm.04)
print(summary(glmm.04))
print(glmm.04.r2)
