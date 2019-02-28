

### ##discriptive statistics - Gender age and total subjects. 

DESCRIPTIVES VARIABLES=iid age
  /STATISTICS=MEAN STDDEV RANGE MIN MAX.


##discriptive statistics - Gender


SORT CASES  BY gender.
SPLIT FILE LAYERED BY gender.

DESCRIPTIVES VARIABLES=age
  /STATISTICS=MIN MAX MEAN  STDDEV.

SPLIT FILE OFF.


CROSSTABS
  /TABLES=iid BY gender
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.


##discriptive statistics -  Race

CROSSTABS
  /TABLES=iid BY race
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.


### General preferences of the subjects they look for in the opposite sex

DESCRIPTIVES VARIABLES=attr1_1 sinc1_1 intel1_1 fun1_1 amb1_1 shar1_1
  /STATISTICS=MEAN STDDEV.

------------------------------------------------------------------------------------------------------------

### Gender preferences of the subjects they look for in the opposite sex

DATASET ACTIVATE DataSet1.
SORT CASES  BY gender.
SPLIT FILE LAYERED BY gender.

DESCRIPTIVES VARIABLES=attr1_1 sinc1_1 intel1_1 fun1_1 amb1_1 shar1_1
  /STATISTICS=MEAN STDDEV VARIANCE RANGE MIN MAX.

SPLIT FILE OFF.


### t test for statistical significant analysis

T-TEST GROUPS=gender(1 0)
  /MISSING=ANALYSIS
  /VARIABLES=attr1_1 amb1_1 shar1_1 sinc1_1 intel1_1 fun1_1  
  /CRITERIA=CI(.95).

### Distirbution by each one of the 6 parameters -  Gender preferences of the subjects they look for in the opposite sex

---Attraction -------------------------------------------------------------------------------------------------

DATASET ACTIVATE DataSet1.
SORT CASES  BY gender.
SPLIT FILE LAYERED BY gender.

FREQUENCIES VARIABLES=attr1_1
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN
  /ORDER=ANALYSIS.

SPLIT FILE OFF.

---since -------------------------------------------------------------------------------------------------

DATASET ACTIVATE DataSet1.
SORT CASES  BY gender.
SPLIT FILE LAYERED BY gender.

DATASET ACTIVATE DataSet1.
FREQUENCIES VARIABLES=sinc1_1
  /ORDER=ANALYSIS.

SPLIT FILE OFF.

---intel -------------------------------------------------------------------------------------------------

DATASET ACTIVATE DataSet1.
SORT CASES  BY gender.
SPLIT FILE LAYERED BY gender.

DATASET ACTIVATE DataSet1.
FREQUENCIES VARIABLES=intel1_1
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN
  /ORDER=ANALYSIS.

SPLIT FILE OFF.

---fun -------------------------------------------------------------------------------------------------

DATASET ACTIVATE DataSet1.
SORT CASES  BY gender.
SPLIT FILE LAYERED BY gender.

DATASET ACTIVATE DataSet1.
FREQUENCIES VARIABLES=fun1_1
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN
  /ORDER=ANALYSIS.

SPLIT FILE OFF.

---amb -------------------------------------------------------------------------------------------------

DATASET ACTIVATE DataSet1.
SORT CASES  BY gender.
SPLIT FILE LAYERED BY gender.

DATASET ACTIVATE DataSet1.
FREQUENCIES VARIABLES=amb1_1
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN
  /ORDER=ANALYSIS.

SPLIT FILE OFF.

---shar -------------------------------------------------------------------------------------------------

DATASET ACTIVATE DataSet1.
SORT CASES  BY gender.
SPLIT FILE LAYERED BY gender.

DATASET ACTIVATE DataSet1.
FREQUENCIES VARIABLES=shar1_1
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN
  /ORDER=ANALYSIS.

SPLIT FILE OFF.

------------------------------------------------------------------------------------------------------------

###Ather sex Preference analysis - What do you think the opposite sex looks for in a date?


DATASET ACTIVATE DataSet1.
SORT CASES  BY gender.
SPLIT FILE LAYERED BY gender.

DESCRIPTIVES VARIABLES=attr2_1 sinc2_1 intel2_1 fun2_1 amb2_1 shar2_1
  /STATISTICS=MEAN STDDEV VARIANCE RANGE MIN MAX.

SPLIT FILE OFF.


------------------------------------------------------------------------------------------------------------
### What men/women really want VS what the other sex think they want? 

DATASET ACTIVATE DataSet1.
SORT CASES  BY gender.
SPLIT FILE LAYERED BY gender.


DESCRIPTIVES VARIABLES=attr1_1 sinc1_1 intel1_1 fun1_1 amb1_1 shar1_1 attr2_1 sinc2_1 intel2_1 
    fun2_1 amb2_1 shar2_1
  /STATISTICS=MEAN STDDEV MIN MAX.

SPLIT FILE OFF.


------------------------------------------------------------------------------------------------------------
### what is the difference between individual yes decision and no decision

T-TEST GROUPS=dec(1 0)
  /MISSING=ANALYSIS
  /VARIABLES=attr  fun shar intel sinc  amb   
  /CRITERIA=CI(.95).


### what is the difference between female or male individual yes decision and no decision


SORT CASES  BY dec.
SPLIT FILE LAYERED BY dec.

T-TEST GROUPS=gender(1 0)
  /MISSING=ANALYSIS
  /VARIABLES=attr  fun shar intel sinc  amb
  /CRITERIA=CI(.95).

SPLIT FILE OFF.


------------------------------------------------------------------------------------------------------------
#### create positive rate

##step one: SUM all the positive selection and put it in a new column - dec_numb

AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=iid
  /dec_o_sum=SUM(dec_o).


##step two: create a column with max partners for each subject.

selecting the round column as max partners for each subject. 

##step three: create the precentage positive rate. if there is max 10 partners and 5 dec yes then the positive rate will be at 50%. 

COMPUTE positive_rate=dec_o_sum * 100 / max_partner_round.
EXECUTE.


------------------------------------------------------------------------------------------------------------
### create a regression between the mean of each variable from the max partners and the positive rate percentage. 

--find the mean for each variable. 

AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=iid
  /attr_o_mean=MEAN(attr_o) 
  /sinc_o_mean=MEAN(sinc_o) 
  /intel_o_mean=MEAN(intel_o) 
  /fun_o_mean=MEAN(fun_o) 
  /amb_o_mean=MEAN(amb_o) 
  /shar_o_mean=MEAN(shar_o) 
  /like_o_mean=MEAN(like_o).


### find the correlation between the positive rate and the mean score of each the six variables. 

CORRELATIONS
  /VARIABLES=Positive_Rate attr_o_mean sinc_o_mean intel_o_mean fun_o_mean amb_o_mean shar_o_mean 
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.


### find the correlation between the positive rate and the mean score of each the six variables on each gender. 

SORT CASES  BY gender.
SPLIT FILE LAYERED BY gender.

CORRELATIONS
  /VARIABLES=Positive_Rate attr_o_mean sinc_o_mean intel_o_mean fun_o_mean amb_o_mean shar_o_mean 
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

SPLIT FILE OFF gender. 

### create a multiple regression to see each variable and his contribute to the model using SEPWISE method. 

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT positive_rate
  /METHOD=STEPWISE attr_o_mean sinc_o_mean intel_o_mean fun_o_mean amb_o_mean shar_o_mean.



### create a graph to the reggression based on the statistical significant values of the model. 
--step 1 - create a multiple regression and save the nustandardize values in a new column. 

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT positive_rate
  /METHOD=ENTER attr_o_mean fun_o_mean
  /SAVE PRED.

--step 2 - create a graph for the standardized values. 
GRAPH
  /SCATTERPLOT(BIVAR)= Unstandardized_values_for_mulitple_regression_scaterdot WITH  positive_rate
  /MISSING=LISTWISE.

------------------------------------------------------------------------------------------------------------
### what is the difference between match and non-match couples? 

T-TEST GROUPS=match(1 0)
  /MISSING=ANALYSIS
  /VARIABLES=fun attr shar intel sinc  amb  
  /CRITERIA=CI(.95).

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## find difference between age group. 
--first create an age group for partners and subjects. 

RECODE age_o (18 thru 24=1) (25 thru 31=2) (32 thru 38=3) (39 thru 45=4) (46 thru 55=5) INTO 
    Age_Group_Partner.
VARIABLE LABELS  Age_Group_Partner 'Age_Group_Partner'.
EXECUTE.

RECODE age (18 thru 24=1) (25 thru 31=2) (32 thru 38=3) (39 thru 45=4) (46 thru 55=5) INTO 
    Age_Group_Subject.
VARIABLE LABELS  Age_Group_Subject 'Age_Group_Subject'.
EXECUTE.


--- frequencies to age groups

SORT CASES  BY gender.
SPLIT FILE LAYERED BY gender.

DATASET ACTIVATE DataSet1.
FREQUENCIES VARIABLES=Age_Group_Subject
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN
  /ORDER=ANALYSIS.

SPLIT FILE OFF gender.

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 20.2.19 -------------------------------
-- create a filter before three way anova test - filter on subjects positive decision and age groups - 1,2,3, race group = black, asian, latino and european 

USE ALL.
COMPUTE filter_$=(dec=1 & (Age_Group_Subject=1 | Age_Group_Subject=2 | Age_Group_Subject=3) & 
    (race=1 | race=2 | race=3 | race=4)).
VARIABLE LABELS filter_$ 'dec=1 & (Age_Group_Subject=1 | Age_Group_Subject=2 | '+
    'Age_Group_Subject=3) & (race=1 | race=2 | race=3 | race=4) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

--- Three way anova on positive decision of the subjects -  gender, age group and race on attractiveness.

UNIANOVA attr BY gender Age_Group_Subject race
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /POSTHOC=Age_Group_Subject race(SCHEFFE) 
  /PLOT=PROFILE(gender*Age_Group_Subject gender*race Age_Group_Subject*race 
    gender*Age_Group_Subject*race)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=gender Age_Group_Subject race gender*Age_Group_Subject gender*race Age_Group_Subject*race 
    gender*Age_Group_Subject*race.


---triple interaction source check - gender, age group and race.  

DATASET ACTIVATE DataSet1.
SORT CASES  BY Age_Group_Subject race.
SPLIT FILE LAYERED BY Age_Group_Subject race.

T-TEST GROUPS=gender(1 0)
  /MISSING=ANALYSIS
  /VARIABLES=attr
  /CRITERIA=CI(.95).

SPLIT FILE OFF.


--- Three way anova on positive decision of the subjects -  gender, age group and race on fun. 

UNIANOVA fun BY gender Age_Group_Subject race
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /POSTHOC=Age_Group_Subject race(SCHEFFE) 
  /PLOT=PROFILE(gender*Age_Group_Subject gender*race Age_Group_Subject*race 
    gender*Age_Group_Subject*race)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=gender Age_Group_Subject race gender*Age_Group_Subject gender*race Age_Group_Subject*race 
    gender*Age_Group_Subject*race.

--double interaction source check - gender and race. 


DATASET ACTIVATE DataSet1.
SORT CASES  BY race.
SPLIT FILE LAYERED BY race.


T-TEST GROUPS=gender(1 0)
  /MISSING=ANALYSIS
  /VARIABLES=fun
  /CRITERIA=CI(.95).

SPLIT FILE OFF.

FILTER OFF.
USE ALL.
EXECUTE.


--------------------------------------------------------------------------------------------------------------------------------------------------------------------
--- Total race dates by other races - to check the positive decision of each race on other race. 
-- European total dates by race

USE ALL.
COMPUTE filter_$=(race=2).
VARIABLE LABELS filter_$ 'race=2 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

FREQUENCIES VARIABLES=race_o
  /PIECHART FREQ
  /ORDER=ANALYSIS.

FILTER OFF.
USE ALL.
EXECUTE.

-- European total positive desicion by race

USE ALL.
COMPUTE filter_$=(race=2 AND dec=1).
VARIABLE LABELS filter_$ 'race=2 AND dec=1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

FREQUENCIES VARIABLES=race_o
  /PIECHART FREQ
  /ORDER=ANALYSIS.

FILTER OFF.
USE ALL.
EXECUTE.



-- African American total dates by race

USE ALL.
COMPUTE filter_$=(race=1).
VARIABLE LABELS filter_$ 'race=2 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

FREQUENCIES VARIABLES=race_o
  /PIECHART FREQ
  /ORDER=ANALYSIS.

FILTER OFF.
USE ALL.
EXECUTE.

-- African American total positive desicion by race

USE ALL.
COMPUTE filter_$=(race=1 AND dec=1).
VARIABLE LABELS filter_$ 'race=1 AND dec=1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

FREQUENCIES VARIABLES=race_o
  /PIECHART FREQ
  /ORDER=ANALYSIS.

FILTER OFF.
USE ALL.
EXECUTE.




-- Latino total dates by race

USE ALL.
COMPUTE filter_$=(race=3).
VARIABLE LABELS filter_$ 'race=3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

FREQUENCIES VARIABLES=race_o
  /PIECHART FREQ
  /ORDER=ANALYSIS.

FILTER OFF.
USE ALL.
EXECUTE.

-- Latino total positive desicion by race

USE ALL.
COMPUTE filter_$=(race=3 AND dec=1).
VARIABLE LABELS filter_$ 'race=3 AND dec=1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

FREQUENCIES VARIABLES=race_o
  /PIECHART FREQ
  /ORDER=ANALYSIS.

FILTER OFF.
USE ALL.
EXECUTE.


-- Asian total dates by race

USE ALL.
COMPUTE filter_$=(race=4).
VARIABLE LABELS filter_$ 'race=4 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

FREQUENCIES VARIABLES=race_o
  /PIECHART FREQ
  /ORDER=ANALYSIS.

FILTER OFF.
USE ALL.
EXECUTE.



-- Asian total positive desicion by race

USE ALL.
COMPUTE filter_$=(race=4 AND dec=1).
VARIABLE LABELS filter_$ 'race=4 AND dec=1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

FREQUENCIES VARIABLES=race_o
  /PIECHART FREQ
  /ORDER=ANALYSIS.

FILTER OFF.
USE ALL.
EXECUTE.
