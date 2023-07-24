ST362_Final_Project_pt1
================
Joseph Aurini - 200544520
2023-07-20

## Part 1: Predicting an MLB Pitcher’s Salary; Pre-Moneyball

I will be generating a simulated data set consisting of a number of
variables that can be used to predict an MLB pitcher’s salary before the
2002 season (i.e. before the Oakland A’s first season following the
“Moneyball” philosophy)

## Creating Simulated Predictors

``` r
# Setting Sample Size
n <- 1111

# Let's start with generating two highly correlated predictors
height <- rnorm(n,mean=190,sd=7) # Height in cm
hand_length <- (height - 75) / (4.8) + rnorm(n,mean=0,sd=.5) # Hand length in cm

# Now, let's generate a categorical predictor and create dummy variables for it
#pitcher_type <- sample(c("starter","reliever","closer"),n,replace = TRUE)
#throwing_arm <- sample(c("righty","lefty"),n,replace=TRUE)
pitching_arsenal <- sample(c("two","three","four"),n,replace=TRUE) # How many different types of pitches they can throw
two_pitches <- ifelse(pitching_arsenal=="two",1,0)
three_pitches <- ifelse(pitching_arsenal=="three",1,0)
four_pitches <- ifelse(pitching_arsenal=="four",1,0)

# And create dummy variables for our categorical predictor
#starter <- ifelse(pitcher_type=="starter",1,0) 
#reliever <- ifelse(pitcher_type=="reliever",1,0)
#closer <- ifelse(pitcher_type=="closer",1,0)
#righty <- ifelse(throwing_arm=="righty",1,0)
#lefty <- ifelse(throwing_arm=="lefty",1,0)

# Generating some more predictors based off real statistical categories
fball_velo <- rnorm(n,mean=93,sd=2) # Average fastball velocity in mph
age <- floor(rnorm(n,mean=27,sd=2.5)) # Player age
win_loss <- rnorm(n,mean=.500,sd=.15) # Win-Loss percentage over the last 3 seasons
op_avg <- rnorm(n,mean=.270,sd=.025) # Opponents batting average when faced
throws_left <- rbinom(n,size=1,prob=.2)

# Generating our most important predictors, we'll call these "The Intangibles"
charisma <- rnorm(n,mean=0,sd=2)
attractiveness <- rnorm(n,mean=5,sd=2)
throws_funny <- rbinom(n,size=1,prob=.025)

# Now let's create our coefficients
b0 <- -650 # Intercept
b1 <- -150 # Two pitches
b2 <- 50 # Three pitches
b3 <- 250 # Four pitches
b4 <- 0 # Height coefficient set to zero due to extremely high correlation with height
b5 <- 12 # Hand length
b6 <- 13 # Fastball velocity
b7 <- -25 # Age
b8 <- 2400 # Win-Loss %
b9 <- -1800 # Opponent's average
b10 <- 250 # Throws Left
b11 <- 2 # Charisma
b12 <- 4 # Charisma-squared
b13 <- 8 # Charisma-cubed
b14 <- 30 # Attractiveness
b15 <- -400 # Throws funny

# Generating our response variable
salary <- (b0 + (b1*two_pitches) + (b2*three_pitches) + (b3*four_pitches) + (b4*height) + (b5*hand_length) + (b6*fball_velo) + (b7*age) + (b8*win_loss) + (b9*op_avg) + (b10*throws_left) + (b11*charisma) + (b12*charisma^2) + (b13*charisma^3) + (b14*attractiveness) + (b15*throws_funny) + rnorm(n,mean=0,sd=1)) * 1000

# Create a data frame with our simulated data
sim_df <- data.frame(salary, pitching_arsenal, two_pitches, three_pitches, four_pitches, height, hand_length, fball_velo, age, win_loss, op_avg, throws_left, charisma, attractiveness, throws_funny)

# Remove data with values that don't make sense
# League min. salary was ~$200,000 at the time and no ones ever thrown over 106 mph in an mlb game
sim_df2 <- subset(sim_df, salary>=200000 & fball_velo<106 & age>=17 & age<=40 & win_loss>=0 & win_loss<=1 & op_avg>=0 & op_avg<=1 & attractiveness>=1 & attractiveness<=10) 

# Extract our simulated data frame
write.csv(sim_df2, "pitcher_salary_sim.csv", row.names=FALSE)
```
