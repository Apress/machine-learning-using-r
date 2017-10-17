###########################################################
# Function concordance : for concordance, discordance, ties
# The function returns Concordance, discordance, and ties
# by taking a glm binomial model result as input.
# It uses optimisation through subsetting
###########################################################

concordance<-function(model)
{
  # Get all actual observations and their fitted values into a frame
  fitted<-data.frame(cbind(model$y,model$fitted.values))
  colnames(fitted)<-c('respvar','score')
  # Subset only ones
  ones<-fitted[fitted[,1]==1,]
  # Subset only zeros
  zeros<-fitted[fitted[,1]==0,]
  
  # Initialise all the values
  pairs_tested<-as.double(nrow(ones))*as.double(nrow(zeros))
  conc<-0
  disc<-0
  
  # Get the values in a for-loop
  for(i in 1:nrow(ones))
  {
    conc<-conc + sum(ones[i,"score"]>zeros[,"score"])
    disc<-disc + sum(ones[i,"score"]<zeros[,"score"])
  }
  # Calculate concordance, discordance and ties
  concordance<-conc/pairs_tested
  discordance<-disc/pairs_tested
  ties_perc<-(1-concordance-discordance)
  return(list("Concordance"=concordance,
              "Discordance"=discordance,
              "Tied"=ties_perc,
              "Pairs"=pairs_tested))
}