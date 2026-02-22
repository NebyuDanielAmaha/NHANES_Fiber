
*Classify pre and post pandemic datasets

generate byte POSTPANDEMIC = .
replace POSTPANDEMIC = 1 if SDDSRVYR == 12
replace POSTPANDEMIC = 0 if SDDSRVYR == 66

rename *, lower

* Use weights for 2 cycles
gen combined_wt = .
replace combined_wt = wtdrd1pp/2 if !missing(wtdrd1pp)
replace combined_wt = wtdrd1/2 if !missing(wtdrd1)


* Set up the survey design
svyset sdmvpsu [pw=combined_wt], strata(sdmvstra) singleunit(centered)


* Drop Impossible values
drop if energy < 500 | energy > 5000
drop if fiber_day1 < 0 | fiber_day1 > 100

* Calculate residuals
svy: regress fiber_day1 energy
predict fiber_resid, residuals
gen fiber_resid10 = fiber_resid / 10
gen fiber_resid5 = fiber_resid /5

* Calculate weighted quartiles BEFORE MI
pctile q_fiber = fiber_resid5 [pw=combined_wt], nq(4)

* List the cut-points
return list

gen fiber_q = .
replace fiber_q = 1 if fiber_day1 <= r(r1)
replace fiber_q = 2 if fiber_day1 > r(r1) & fiber_day1 <= r(r2)
replace fiber_q = 3 if fiber_day1 > r(r2) & fiber_day1 <= r(r3)
replace fiber_q = 4 if fiber_day1 > r(r3)
label define q 1 "Q1 (lowest)" 2 "Q2" 3 "Q3" 4 "Q4 (highest)"
label values fiber_q q




* Check missing
misstable summarize age_category gender race education_level marital_status income_category bmi_category bmi_overweight sleep_duration prescribed_meds vigorous_exe alc_current current_smoker hscrp_category depression_binary depression_cat energy

* MCAR test
mcartest income_category prescribed_meds alc_current current_smoker education_level marital_status vigorous_exe 


* Save appended dataset
save "NHANES_APPENDED.dta", replace

* Multiple Imputation to address the missing
* Declare
mi set mlong
* Register
mi register imputed income_category prescribed_meds alc_current current_smoker education_level marital_status vigorous_exe
mi register regular age_category gender race bmi_category bmi_overweight sleep_duration hscrp_category depression_binary depression_cat postpandemic fiber_q

* Perform
mi impute chained (mlogit) income_category education_level marital_status (logit) current_smoker alc_current vigorous_exe prescribed_meds = age_category gender race bmi_cat bmi_overweight sleep_duration hscrp_category depression_binary depression_cat, add(20) rseed(12345)

* Save imputed appended dataset
save "NHANES_APPENDED_IMPUTED_NOV08.dta", replace

* Set up the survey design
mi svyset sdmvpsu [pw=combined_wt], strata(sdmvstra) singleunit(centered)


gen fiber10 = fiber_day1 / 10


* TABLE 1

*Table 1 stratified by post-pandemic (0 = pre, 1 = post) ---*
mi svyset sdmvpsu [pw=combined_wt], strata(sdmvstra) singleunit(centered)

* Continuous outcomes
mi estimate, esampvaryok: svy, over(postpandemic): mean fiber_day1
mi estimate, esampvaryok: svy, over(postpandemic): mean energy

* Categorical outcomes
mi estimate, esampvaryok: svy, over(postpandemic): proportion gender
mi estimate, esampvaryok: svy, over(postpandemic): proportion age_category
mi estimate, esampvaryok: svy, over(postpandemic): proportion race
mi estimate, esampvaryok: svy, over(postpandemic): proportion education_level
mi estimate, esampvaryok: svy, over(postpandemic): proportion marital_status
mi estimate, esampvaryok: svy, over(postpandemic): proportion income_category
mi estimate, esampvaryok: svy, over(postpandemic): proportion bmi_overweight
mi estimate, esampvaryok: svy, over(postpandemic): proportion sleep_duration
mi estimate, esampvaryok: svy, over(postpandemic): proportion prescribed_meds
mi estimate, esampvaryok: svy, over(postpandemic): proportion vigorous_exe
mi estimate, esampvaryok: svy, over(postpandemic): proportion alc_current
mi estimate, esampvaryok: svy, over(postpandemic): proportion current_smoker
mi estimate, esampvaryok: svy, over(postpandemic): proportion hscrp_category
mi estimate, esampvaryok: svy, over(postpandemic): proportion depression_binary
mi estimate, esampvaryok: svy, over(postpandemic): proportion depression_cat


*mi estimate, esampvaryok: glm depression_binary c.fiber_resid10 ib1.gender ib3.race ib5.education_level i.age_category i.marital_status ib3.income_category i.bmi_overweight i.sleep_duration i.prescribed_meds i.vigorous_exe i.alc_current i.current_smoker i.hscrp_category, family(poisson) link(log) eform
*mi estimate, eform

* TABLE 2: Association between dietary fiber and depression by pandemic period

*pooled
mi estimate, esampvaryok:glm depression_binary c.fiber_resid5 ib1.gender ib3.race ib5.education_level i.age_category i.marital_status ib3.income_category i.bmi_overweight i.sleep_duration i.prescribed_meds i.vigorous_exe i.alc_current i.current_smoker i.hscrp_category, family(poisson) link(log) eform
mi estimate, eform

*pre and post separate
mi estimate: svy, subpop(if postpandemic == 0): glm depression_binary c.fiber_resid5 ib1.gender ib3.race ib5.education_level i.age_category i.marital_status ib3.income_category i.bmi_overweight i.sleep_duration i.prescribed_meds i.vigorous_exe i.alc_current i.current_smoker i.hscrp_category, family(poisson) link(log) eform
mi estimate, eform

mi estimate: svy, subpop(if postpandemic == 1): glm depression_binary c.fiber_resid5 ib1.gender ib3.race ib5.education_level i.age_category i.marital_status ib3.income_category i.bmi_overweight i.sleep_duration i.prescribed_meds i.vigorous_exe i.alc_current i.current_smoker i.hscrp_category, family(poisson) link(log) eform
mi estimate, eform


* TABLE 3 Interaction table

* Test for interaction with time period and gender
mi estimate: svy: glm depression_binary c.fiber_resid5##i.postpandemic##i.gender ib3.race ib5.education_level i.age_category i.marital_status ib3.income_category i.bmi_overweight i.sleep_duration i.prescribed_meds i.vigorous_exe i.alc_current i.current_smoker i.hscrp_category, family(poisson) link(log) eform
mi estimate, eform



* TABLE 4: Stratified analysis by gender and period

* Prepandemic

mi estimate: svy, subpop(if gender == 1 & postpandemic==0): glm depression_binary c.fiber_resid5 ib3.race ib5.education_level i.age_category i.marital_status ib3.income_category i.bmi_overweight i.sleep_duration i.prescribed_meds i.vigorous_exe i.alc_current i.current_smoker i.hscrp_category, family(poisson) link(log) eform
mi estimate, eform

mi estimate: svy, subpop(if gender == 2 & postpandemic==0): glm depression_binary c.fiber_resid5 ib3.race ib5.education_level i.age_category i.marital_status ib3.income_category i.bmi_overweight i.sleep_duration i.prescribed_meds i.vigorous_exe i.alc_current i.current_smoker i.hscrp_category, family(poisson) link(log) eform
mi estimate, eform


* Postpandemic 

mi estimate: svy, subpop(if gender == 1 & postpandemic==1): glm depression_binary c.fiber_resid5 ib3.race ib5.education_level i.age_category i.marital_status ib3.income_category i.bmi_overweight i.sleep_duration i.prescribed_meds i.vigorous_exe i.alc_current i.current_smoker i.hscrp_category, family(poisson) link(log) eform
mi estimate, eform

mi estimate: svy, subpop(if gender == 2 & postpandemic==1): glm depression_binary c.fiber_resid5 ib3.race ib5.education_level i.age_category i.marital_status ib3.income_category i.bmi_overweight i.sleep_duration i.prescribed_meds i.vigorous_exe i.alc_current i.current_smoker i.hscrp_category, family(poisson) link(log) eform
mi estimate, eform

* Figure 2
twoway (lowess phq9_total_score fiber_day1 if postpandemic==0, bwidth(0.8) lcolor(blue) lwidth(medium))(lowess phq9_total_score fiber_day1 if postpandemic==1, bwidth(0.8) lcolor(red) lwidth(medium)), legend(order(1 "Pre-pandemic" 2 "Pandemic") position(12) ring(0)) ytitle("PHQ-9 total score") xtitle("Fiber intake (grams/day)") yscale(range(0 .)) xlabel(0(10)100) ylabel(0(1)7)

* Figure 3
* Fit the MI model
margins gender#postpandemic, at(fiber_resid5=(0(1)10))
marginsplot, x(fiber_resid5) recast(line) plot1opts(lcolor(blue) lpattern(solid) lwidth(medium) ciopts(fcolor(blue%20))) plot2opts(lcolor(red) lpattern(solid) lwidth(medium) ciopts(fcolor(red%20))) plot3opts(lcolor(blue) lpattern(dash) lwidth(medium) ciopts(fcolor(blue%20))) plot4opts(lcolor(red) lpattern(dash) lwidth(medium) ciopts(fcolor(red%20))) legend(order(1 "Male – Pre-pandemic" 2 "Female – Pre-pandemic" 3 "Male – Pandemic" 4 "Female – Pandemic")) ytitle("Predicted Probability of Depression") xtitle("Dietary Fiber (residualized, g/day)") title("Predicted Depression by Fiber, Gender, and Pandemic Period")

* TABLE 5: Sensitivity analysis by quartiles

mi estimate, esampvaryok: svy: glm depression_binary ib4.fiber_q ib1.gender ib3.race ib5.education_level i.age_category i.marital_status ib3.income_category i.bmi_overweight i.sleep_duration i.prescribed_meds i.vigorous_exe i.alc_current i.current_smoker i.hscrp_category, family(poisson) link(log) eform
mi estimate, eform

* Compare pre and pandemic periods

mi estimate: svy, subpop(if postpandemic==0): glm depression_binary ib4.fiber_q ib1.gender ib3.race ib5.education_level i.age_category i.marital_status ib3.income_category i.bmi_overweight i.sleep_duration i.prescribed_meds i.vigorous_exe i.alc_current i.current_smoker i.hscrp_category, family(poisson) link(log) eform
mi estimate, eform

mi estimate: svy, subpop(if postpandemic==1): glm depression_binary ib4.fiber_q ib1.gender ib3.race ib5.education_level i.age_category i.marital_status ib3.income_category i.bmi_overweight i.sleep_duration i.prescribed_meds i.vigorous_exe i.alc_current i.current_smoker i.hscrp_category, family(poisson) link(log) eform
mi estimate, eform
