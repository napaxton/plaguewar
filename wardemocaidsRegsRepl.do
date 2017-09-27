version 11
use "WarHIV2007Repl.dta"

//Set the data as panel data
sort CTRY year
xtset CTRY year
set more off


//Summary Statistics
sum estimate d.estimate intwar civwar ethwar polity2 gdppercap offdevassistandoffaidcurrentus ///
	popdens urbanpopprct refsasylum000 healthExpPCap LOG healthpercapcurr if year >= 1990 
sum estimate d.estimate intwar civwar ethwar polity2 gdppercap offdevassistandoffaidcurrentus ///
	popdens urbanpopprct refsasylum000 healthExpPCap healthpercapcurr if year >= 1990 & africayes==1	


sort regtrans
by regtrans: sum change if (regtrans >=-5 & regtrans <=10)
sort CTRY year
//incomegroup == 1, 2, 3 (Low, Low Middle, High Middle) => created var "devgyes"
//region== 1,3,4 (West, East, South Africa), 2 = North Africa
//hipc 
// if region == 1 | region==3 | region== 4
// for var regionwbnum, 1= .., 2= East Asia Pac, 3=Eur Cent Asia, 4= LA and Carib, 5= MENA, 6= S Asia, 7= SSA
// using regionwbnum, created a "africayes" for subsetting the African cases

//Multivariate regressions. First I do African states, and then the rest of the developing countries
sort CTRY year
//OLS with robust SE, no accounting for country or time (except for the lagged variable)
//reg estimate polity2 l3.intwar l3.civwar l3.ethwar if africayes == 1, robust
//using the Africa subset of data (WarDemHIV2007Af.dta)
quietly reg estimate l3.civwar l3.ethwar l3.intwar polity2 if africayes==1, robust
	eststo m1AfrOLSrse
//Beck and Katz PCSE model
quietly xtpcse estimate l3.civwar l3.ethwar l3.intwar polity2 if africayes==1
	eststo m1AfrOLSpcse
//Basic fixed effects model, with robust SEs
quietly xtreg estimate l3.civwar l3.ethwar l3.intwar polity2 if africayes==1,fe robust
	eststo m1AfrFErse
//Fixed effects model with BeckKatz PCSE
quietly xtpcse estimate l3.civwar l3.ethwar l3.intwar polity2 i.CTRY if africayes==1
	eststo m1AfrFEpcse

//Now for all developing countries 
//OLS with robust SE, no accounting for country or time (except for the lagged variable)
* use "/Users/nathanpaxton/Dropbox/quantwork/WarDemocAIDS/WarDemHIV2007Devg.dta"
//sort CTRY year
quietly reg estimate l3.civwar l3.ethwar l3.intwar polity2 africayes , robust
	eststo m1DevOLSrse
//Beck and Katz PCSE model 
quietly xtpcse estimate l3.civwar l3.ethwar l3.intwar polity2 africayes
	eststo m1DevOLSpcse
//Basic fixed effects model, with robust SEs; Africa not included b/c FE
quietly xtreg estimate l3.civwar l3.ethwar l3.intwar polity2 ,fe robust
	eststo m1DevFErse
//Fixed effects model with BeckKatz PCSE //africa dummy not sensible since fixed effects estimates individual country intercepts
quietly xtpcse estimate l3.civwar l3.ethwar l3.intwar polity2 i.CTRY 
	eststo m1DevFEpcse
	


//Model 2
//Model 2 runs the same tests as Model 1, but with control variables (with a fairly high total N) included
// gdpgrowth (lin and log forms) both dropped on 15 Nov, not only because  didn't have a huge reason to keep them in, but they also made xtpcse choke
//African countries only
quietly reg estimate l3.civwar l3.ethwar l3.intwar polity2 loggdpcap ODAlog refsasylum000 LOGhealthpercapcurr if africayes==1, robust
	eststo m2AfrOLSrse
quietly xtpcse estimate l3.civwar l3.ethwar l3.intwar polity2 loggdpcap ODAlog refsasylum000 LOGhealthpercapcurr if africayes==1, pairwise
	eststo m2AfrOLSpcse
quietly xtreg estimate l3.civwar l3.ethwar l3.intwar polity2 loggdpcap ODAlog refsasylum000 LOGhealthpercapcurr if africayes==1, fe robust
	eststo m2AfrFErse
quietly xtpcse estimate l3.civwar l3.ethwar l3.intwar polity2 loggdpcap ODAlog refsasylum000 LOGhealthpercapcurr i.CTRY if africayes==1, pairwise
	eststo m2AfrFEpcse

estout *Afr* , cells(b(star fmt(3)) se(par fmt(3)) ) label wrap starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001) drop (*.CTRY) stats(r2 rmse N_g N) style (mmd)
esttab *Afr* using "/Users/nathanpaxton/Dropbox/quantwork/WarDemocAIDS/SSAtable.tex", cells(b(star fmt(3)) se(par fmt(3)) ) label wrap starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001) drop (*.CTRY _cons) scalars("N N" "N_g Countries" "r2 R^2" "rmse Root Mean Sq. Err." "rho Rho") mtitles substitute(_ \_) replace
esttab *Afr* using "/Users/nathanpaxton/Dropbox/quantwork/WarDemocAIDS/SSAtable.csv", cells(b(star fmt(3)) se(par fmt(3)) ) label wrap starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001) drop (*.CTRY _cons) scalars("N N" "N_g Countries" "r2 R^2" "rmse Root Mean Sq. Err." "rho Rho") mtitles substitute(_ \_) replace

// If want MMD format output to file, use:
estout m*Afr* using "/Users/nathanpaxton/Dropbox/quantwork/WarDemocAIDS/Diffstable.mmd", cells(b(star fmt(3)) se(par fmt(3)) ) label wrap starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001) drop (*.CTRY) stats(r2 rmse N_g N) style (mmd) replace

//All Dev'g Countries
quietly reg estimate l3.civwar l3.ethwar l3.intwar polity2 loggdpcap ODAlog refsasylum000 LOGhealthpercapcurr africayes, robust
	eststo m2DevOLSrse
quietly xtpcse estimate l3.civwar l3.ethwar l3.intwar polity2 loggdpcap ODAlog refsasylum000 LOGhealthpercapcurr africayes, pairwise
	eststo m2DevOLSpcse
quietly xtreg estimate l3.civwar l3.ethwar l3.intwar polity2 loggdpcap ODAlog refsasylum000 LOGhealthpercapcurr, fe robust
	eststo m2DevFErse
quietly xtpcse estimate l3.civwar l3.ethwar l3.intwar polity2 loggdpcap ODAlog refsasylum000 LOGhealthpercapcurr i.CTRY, pairwise
	eststo m2DevFEpcse


estout *Dev* , cells(b(star fmt(3)) se(par fmt(3)) ) label wrap starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001) drop (*.CTRY _cons) stats(r2 rmse N_g N) style (mmd)
esttab *Dev* using "/Users/nathanpaxton/Dropbox/quantwork/WarDemocAIDS/Devgtable.tex", cells(b(star fmt(3)) se(par fmt(3)) ) label wrap starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001) drop (*.CTRY _cons) scalars("N N" "N_g Countries" "r2 R^2" "rmse Root Mean Sq. Err." "rho Rho") mtitles substitute(_ \_) replace

// Model differencing
fvset base 1 CTRY
fvset base 1 year

quietly xtpcse diffest l3.civwar l3.ethwar l3.intwar d.polity2 d.loggdpcap d.ODAlog  d.refsasylum000 d.LOGhealthpercapcurr  if africayes==1
	eststo DIFAfrOLSpcse
quietly xtpcse diffest l3.civwar l3.ethwar l3.intwar d.polity2 d.loggdpcap d.ODAlog d.refsasylum000 d.LOGhealthpercapcurr  i.CTRY if africayes==1
	eststo DIFAfrFEpcse
quietly xtpcse diffest l3.civwar l3.ethwar l3.intwar d.polity2 d.loggdpcap d.ODAlog d.refsasylum000 d.LOGhealthpercapcurr  africayes, pairwise
	eststo DIFDevOLSpcse
quietly xtpcse diffest l3.civwar l3.ethwar l3.intwar d.polity2 d.loggdpcap d.ODAlog d.refsasylum000 d.LOGhealthpercapcurr  i.CTRY, pairwise
	eststo DIFDevFEpcse


estout DIF* , cells(b(star fmt(3)) se(par fmt(3)) ) label wrap starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001) drop (*.CTRY  _cons) stats(r2 rmse N_g N) style (mmd)
esttab DIF* using "/Users/nathanpaxton/Dropbox/quantwork/WarDemocAIDS/Diffstable.tex", cells(b(star fmt(3)) se(par fmt(3)) ) label wrap starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001) drop (*.CTRY _cons) scalars("N N" "N_g Countries" "r2 R^2" "rmse Root Mean Sq. Err." "rho Rho") mtitles substitute(_ \_) replace

