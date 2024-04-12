/*******************************************************************************

 Author of the script: Phyllisa Joseph
 Date 		     : 8-04-2024
  
  
  note: I comment beside each line of code and I include white
  spaces so it looks neater.
********************************************************************************/

clear all
set more off

*ssc install fre                                                                // I use this instead of tab as i can understand the value labels and also see missings, without using , m option
*ssc install estout								// In order to export as latex tables
*----------------------         Setting globals             -------------------*

global path    "/Users/phyllisajoseph/Downloads/GPRL_StataAssessment_2024"
global inputs  "$path/data"

cap    mkdir   "$path/output"
global outputs "$path/output"


global assets  "animaltype toolcode durablegood_code"
global kessler "tired nervous sonervous hopeless restless sorestless depressed everythingeffort nothingcheerup worthless" 

/*-----------------------------------------------------------------------------*

				PART 1



*-----------  Q1: At what level is each dataset uniquely identified  ----------*/

*~> Demographics
use "$inputs/demographics.dta", clear       
 
sort       hhid								      					// by looking at the data i think wave, hhid and hhmid will uniquely identify each observations, next step is to check this
duplicates re wave hhid hhmid  
assert     r(N)== r(unique_value)                 								// checking if these three columns would together uniquely identify observations: ans-it does!!

* For demographics, wave hhid hhmid are unique identifiers

*~> Assets
use "$inputs/assets.dta", clear      
      
sort 	   hhid
duplicates re wave hhid Asset_Type InstanceNumber
assert     r(N)== r(unique_value)                 								// checking if these four columns would together uniquely identify observations: ans-it does!!

* For assets, wave, hhid, Asset_Type and InstanceNumber are unique identifiers

*~> Depression
use "$inputs/depression.dta", clear   
   
sort       hhid								     
duplicates re wave hhid hhmid  
assert     r(N)== r(unique_value)                								// checking if these three columns would together uniquely identify observations: ans-it does!!

* For deppression, wave hhid hhmid are unique identifiers

*-----------  Q2: Calculate a variable proxying for household size  -----------*

use "$inputs/demographics.dta", clear   

by         hhid : egen hhsize = max(hhmid)        if wave==1        		// I do this with the assumption that hhmid goes in order for wave 1, that is if max is 5 then 1 to 3 and 4 are also present in wave 1
by         hhid : egen hhsize_check= count(hhmid) if wave==1        		// I also do this to check if this assumptioin is right, or is it for instance 5 but actually only three households are survey and the hhmid is simply written as 5 

assert     hhsize == hhsize_check						// I see that indeed wave 1, the coding for hhmid goes in order is max of hhid = the count of number if oservations within each hh in wave 1
bys        hhid (wave) : replace hhsize=hhsize[1]                               // I applhy this hhsize based on wave 1 to wave 2 obs as well, as assuming its the same.
drop       hhsize_check                                                         // dropping this as we only need hhsize
label      variable hhsize "Household size based on wave 1"
*tab      hhsize, m
tostring   hhid, replace
gen        length_hhid=length(hhid)
assert     length==9
drop       length

tempfile demographics
save     `demographics'

*-------------   Q3: Calculate the monetary value of all assets   -------------*
use "$inputs/assets.dta", clear  

count if   currentvalue==.                                                      // 52 perc missing
count if   animaltype==. & toolcode==. & durablegood_code==.                    // 0 missing 

*~> cleaning
*fre animaltype 
*fre toolcode`
replace    toolcode = . if inlist(toolcode, -1, -2)
*fre durablegood_code
                                                                                // these are labeled as missing1 and missing 2 I assume they are missing
*~> Imputed currentvalue
gen        imp_currentvalue = currentvalue

foreach    asset in $assets{          						// Loop to replace missing current value for median currentalue 
		   levelsof `asset', local(type)                                // https://www.stata.com/manuals13/plevelsof.pdf
		   foreach  i in `type' {
				    qui sum  currentvalue  if   `asset' ==`i', detail    		// getting the median current value for each type of item within each type of asset eg, median currentvalue of chicken in the asset animaltype
				    local    median_`i'=   r(p50)                        		// storing the median current value
					
				    replace  imp_currentvalue   =  `median_`i''   if `asset' ==`i' & inlist(imp_currentvalue,.,.d)		 // replacing missing current value within each item with the median  
		 }
}

count if   imp_currentvalue==.
assert     r(N)==0

*--------------   Q4: total monetary value for each observation   -------------*

assert     quantity!=.
gen        totalvalue= quantity* imp_currentvalue
assert     totalvalue !=.

*---------------------  Q5:household-wave level data set   --------------------*


*fre Asset_Type 								// I can use the variable Asset_Type to do this // no missings good, types= Animals, tools and durables, so will use this to create the total value vairables for each type

*~> Creating data set with 2 obs per hh                                                             
bys        hhid wave Asset_Type : egen totalv_   =  total(totalvalue)           // foreach assettyp hh and wave getting the totalvalue
assert     totalv_ != .                       					 // should not be missing

keep       hhid wave Asset_Type totalv_                                         // keeping only those variables needed

duplicates drop hhid wave Asset_Type, force                                     // dropping duplicates need max only 2 observations per hh, i.e, one observation for each wave per hh

reshape    wide totalv_ , i(hhid wave) j(Asset_Type)                            // reshaping to get colums totalv_animals totalv_tools & totalv_durables

rename     totalv_1 totalv_animals						// giveing better names
rename     totalv_2 totalv_tools
rename     totalv_3 totalv_durables

label      variable totalv_animals  "Total value of all animals"
label      variable totalv_tools    "Total value of all tools"
label      variable totalv_durables "Total value of all durables"

duplicates re hhid wave
assert 	   r(N)==r(unique_value)                                                // checking

*~> Total value of all assets
egen long  totalv_assets = rowtotal(totalv_*)
label      variable totalv_assets "Total value of assets"

gen        length_hhid=length(hhid)
assert     length==10
replace    hhid= substr(hhid, 2,10)
gen        length2= length(hhid)
assert     length2==9
drop       length_hhid length2
	
tempfile   assets
save      `assets'

*-----------------------  Q6: Construct the Kessler score  --------------------*

use "$inputs/depression.dta", clear

egen       rowmiss=rowmiss($kessler)   
assert 	   rowmiss==0 if wave==2                                                // all of them with atleast 1 col missing comes from wave 1, nothin in wave2 is missing	 // will impute missing using wave 1s control median and treatment median based on which group the household belongs to so for this need demographics data

	
tostring   hhid, replace
gen        length_hhid=length(hhid)
assert     length==9
drop       length

/*
Note: Incase there are missing in one of the 10 Kessler variables I impute using 
the following method:

1. I check for each row how many kessler variables are missing out of 10
2. If all 10 components are missing I keep the kessler score as missing (22 obs)
3. If individual has a valid response to atleast one component then the missing 
   values are imputed at the random assignment group median. That is if individual
   belong to control group then I replace the missing compenent with the median 
   value of the component in the control group. 
   
A similar approach was followed by
Kling JR, Liebman JB, Katz LF. Experimental analysis of neighborhood effects. 
Econometrica. 2007 Jan;75(1):83-119.
*/						

merge 1:1  wave hhid hhmid using `demographics', keepusing(treat_hh) assert(2 3) keep(3) nogen

foreach    i in $kessler {
		   forval j =0/1{								// loop gets the median for each component for the control and treated groups
				  sum `i' if treat_hh==`j', detail                              // to get treated group median and control group median
				  local median_`j' = r(p50)
				  replace `i'= `median_`j'' if missing(`i') & rowmiss!=0 &rowmiss!=10 & treat_hh==`j'
				}
}


drop 	   treat_hh
egen       rowmiss2=rowmiss($kessler)
assert     inlist(rowmiss2,0,10)
drop       rowmiss2 rowmiss

egen       kessler_score =rowtotal($kessler)                                    // K10 Total score is based on the sum of K10 item 01 through 10: https://www.corc.uk.net/media/1275/kessler10_manual.pdf
replace    kessler_score=. if kessler_score==0
assert     (kessler_score>=10 & kessler_score<=50) |kessler_score==.            // Score range from 10 to 50, 22 obs it is empty

gen        kessler_categories=.						        // https://www.corc.uk.net/media/1275/kessler10_manual.pdf, i get the ranges of kassler score for each category from this document provided
replace    kessler_categories =1 if kessler_score>=10 & kessler_score<=19 & kessler_categories==.        
replace    kessler_categories =2 if kessler_score>=20 & kessler_score<=24 & kessler_categories==.
replace    kessler_categories =3 if kessler_score>=25 & kessler_score<=29 & kessler_categories==.
replace    kessler_categories =4 if kessler_score>=30 & kessler_score<=50 & kessler_categories==.
assert    !missing(kessler_categories) | kessler_score==.

label      define   kessler 1 "no significant depression" 2 "mild depression" 3 "moderate depression" 4 "severe depression" 
label      values   kessler_categories kessler
label      variable kessler_score "Kessler score (10-50)"
label      variable kessler_categories "kessler categories (4 means sever depression)"


*----------------------  Q7: Constructing a single dataset  -------------------*

merge 1:1  wave hhid hhmid using `demographics',  assert(2 3) keep(3) nogen
merge m:1  wave hhid using `assets',  assert(1 2 3) keep(3) nogen               // (20 observations have dep data but no asset data, 43 observation in assets notpresent in depression)

*keep if    inlist(hhmid, 1,2)                                                  // im keeping these as only they were invited for treatment

gen       women= (gender==5)							// I create dummy variable women useful in the next part
label     define gender 1 "female" 0 "male"
label     values women gender
label     variable women "Gender(=1 female)"

gen       not_single=inlist(maritalstatus, 1,2,7)				// I create dummy variable for partner useful in the next part
label     define single 1"Not single" 0 "Single "
label     values not_single single
label     variable not_single "Partner(=1 not single)"



save "$outputs/demographic_asset_depression", replace				// this is data set that will be used for part 2

/*-----------------------------------------------------------------------------*

				PART 2


*------------  Q1: Explore the relationship between depression and  -----------*/




use "$outputs/demographic_asset_depression", clear
keep if   wave==1

*~> (1)  Household wealth, proxied by total asset value: kessler_categories & totalv_assets


* Top-codiing total asset value to remove outliers
sum 	  totalv_assets, detail
local     p99=r(p99)
replace   totalv_assets=`p99' if totalv_assets > `p99'

* Scatter Plot
set       scheme s1color											// generating a scatter plot to see relationship between total value of assets and  Kessler score
scatter   kessler_score totalv_assets   || lfit kessler_score totalv_assets, ///
          title("Scatter Plot of Kessler Score & Total Asset Value") ///
          xtitle("Total Asset Value") ytitle("Kessler Score") ///
	      yscale(range(10, 60)) ///
          ylabel(10(10)60, format(%9.0gc))
gr export "$outputs/Assets_kscore.png", as(png) replace wid(2000) hei(2000)


*~> (2)  A household of demographical characteristics
local hhvars "totalv_assets women not_single"									// variables used in regression
local caption "Correlation between Depression and Demogrphic variables"

local footnote "Column 1,2,3 show linear regression between Kessler Scores and total asset value, gender and partner respectively. The total asset value has been top-coded at the 99th percentile to remove the influence of outliers. Gender is a dummy variable which takes 1 for female and 0 for male. Variable partner is defined as a dummy variable which takes 1 if the person is married, in a consensual relationship or betrothed and takes 0 if the respondent is widowed, divorced or never married. Depedant variable depression is measured using Kessler score."
   
  eststo clear
  local ester=1
  local eststr ""
  foreach var of varlist `hhvars' {
  	
  	reg kessler_score  `var'											// running regression one by one for each of the var in the local hhvars
	qui estadd scalar N2=e(N)											// getting total observations
    qui summ kessler_score
    qui estadd scalar mean_out=`r(mean)'										// getting mean of outcome=kessler score
     
	qui est store E`ester'
	qui estadd local space = ""
	
					local  stats " j mean_out  N2 " 
					local  statlabels `" "\hline"   "Outcome mean" "N" "'
					local fmt 0 3 0 
					local layout @ @ @ 
			
				local eststr "`eststr' E`ester'"
				local ++ester
		
	}
	 local lblvar : variable label kessler_score
	 local header " & \multicolumn{3}{c} {Depression}"
	 local headline " \cmidrule(lr){2-4}"
	
	esttab `eststr' using "$outputs/hh_characteristic.tex", replace frag style(tex) ///
	    keep(`hhvars') label ///
		noabbrev  nogaps ///
		stats( `stats', fmt( `fmt') /// 
		labels( `statlabels') layout (`layout')) ///
		booktabs nonote  nonum nomtitles nogaps eqlabels(none) star(* 0.10 ** 0.05 *** 0.01) ///
		b(%9.3f) se(%9.3f) noline ///
				prehead("\begin{table}[H] \centering \onehalfspacing \begin{adjustbox}{max width=1\linewidth, max totalheight=1\textheight} \begin{threeparttable} \caption{`caption'} \label{`label'} { \begin{tabular}{l*{`ester'}{c}} \midrule \midrule `header' \\ `headline' ") /// 
				posthead( "\midrule \\") ///
		postfoot(" \hline \end{tabular} } \begin{tablenotes}[flushleft] \small \item `footnote' \end{tablenotes} \end{threeparttable} \end{adjustbox} \end{table}") 
	

		
/*---------  Q2: Were the GT sessions effective at reducing depression ---------*	
									&
*- Q3: Did the effect of the GT sessions on depression vary for men and women -*/	

use "$outputs/demographic_asset_depression", clear

keep if   wave==2
	
gen   int_female_treated= women*treat_hh										// interaction of gender and treatment status


local treat_reg "treat_hh"												// setting locals for explanatory varriable for col1 and col2 and variables for interaction regression
local int_reg "women treat_hh int_female_treated"
local caption "Effect of Group Therapy on Depression"

local footnote "Column 1 shows the treatment effect on household and Column 2 shows heterogeneous treatments effects by gender. Dependent variable is depression measured through Kessler score. Standard errors in all regressions are clustered at the household level (treatment level)"
   
    eststo clear
	local ester=1
	local eststr ""
  foreach var in treat_reg int_reg  {
  	
  	reg kessler_score  ``var'', vce(cluster hhid)								// running regressing for each treat regression col1  and regression with int reg col2
	qui estadd scalar N2=e(N)										// getting total obs
    qui summ kessler_score
    qui estadd scalar mean_out=`r(mean)'									// mean of outcome
     
	qui est store E`ester'											// next few lines help in formatting the regression table to latex
	qui estadd local space = ""
	
					local  stats " space mean_out  N2 " 
					local  statlabels `" " "  "Outcome mean" "N" "'
					local fmt 0 3 0 
					local layout @ @ @ 
			
				local eststr "`eststr' E`ester'"
				local ++ester
		
	}
	 local lblvar : variable label kessler_score
	 local header " & \multicolumn{2}{c} {Depression}"
	 local headline " \cmidrule(lr){2-3}"
	
	esttab `eststr' using "$outputs/treat_table.tex", replace frag style(tex) ///
	    label ///
		noabbrev  nogaps ///
		stats( `stats', fmt( `fmt') /// 
		labels( `statlabels') layout (`layout')) ///
		booktabs nonote  nonum nomtitles nogaps eqlabels(none) star(* 0.10 ** 0.05 *** 0.01) ///
		b(%9.3f) se(%9.3f) noline ///
				prehead("\begin{table}[H] \centering \onehalfspacing \begin{adjustbox}{max width=1\linewidth, max totalheight=1\textheight} \begin{threeparttable} \caption{`caption'} \label{`label'} { \begin{tabular}{l*{`ester'}{c}} \midrule \midrule `header' \\ `headline' ") /// 
				posthead( "\midrule \\") ///
		postfoot(" \hline \end{tabular} } \begin{tablenotes}[flushleft] \small \item `footnote' \end{tablenotes} \end{threeparttable} \end{adjustbox} \end{table}") 
		


exit




















