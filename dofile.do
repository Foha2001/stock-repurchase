//*********Stock repurchase, business cycle and stock market*****///
// *********volatility: Evidence from linear causality tests*****///
//graphic calculator
keep if ïlocation=="USA"
tsset date
gen GINDP=ln(INDP[_n]/INDP[_n-1])
keep if date>= tq(1998Q1)
tsline GINDP
// graph with shaded area
nbercycles vari, file(test.do) replace
**********************BEGIN*****************************
//******program for preparing unit root test*******//
// and graphing business cycle in each country // 
clear
#delimit ; // pour delimiter les longs lignes 
use "C:\d\FOUED\foued\travaux\habilitation\growth ; 
and payout\data industrial production.dta", clear
#delimit cr   // fin de long ligne 
snapshot save, label("Snapshot 1") 
		// *******for USA**********
		keep if ïlocation=="USA"
		drop in 1
		tsset date
		drop in 1/315
		tsline gINDP		
		// *******for GBR**********
		snapshot restore 1
		keep if ïlocation=="GBR"
		tsset date
		drop in 1/168
		tsline gINDP
		//******************
				// *******for JPN**********
		snapshot restore 1
		keep if ïlocation=="JPN"
		tsset date
		drop in 1/172
		tsline gINDP
           // *******for CAN**********
		snapshot restore 1
		keep if ïlocation=="CAN"
		tsset date
		drop in 1/148
		tsline gINDP
		//******************

************************END****************************
#delimit; 
use "C:\d\FOUED\foued\travaux\habilitation\;
growh and payout\data repurchase min.dta", clear 
#delimit cr
#delimit; 
use "C:\d\FOUED\foued\travaux\habilitation\growh;
and payout\data repurchase ave.dta", clear
#delimit; 
//******create testrepurchaseMIN files******//
//******************************************//
clear all
set more off 
use "C:\d\FOUED\foued\travaux\habilitation\growh and payout\data repurchase min.dta", clear
keep date llsearepurchasesp500min llsearepurchasetopixmin llsearepurchasetsxmin llsearepurchaseftsemin
foreach k in sp500min topixmin tsxmin ftsemin {
preserve
keep date llsearepurchase`k'
rename llsearepurchase`k' repurchaseMIN
save testrepurchasemin_`k', replace                			
restore
 }
shell ren testrepurchasemin_sp500min.dta  testrepurchasemin_USA.dta
shell ren testrepurchasemin_topixmin.dta testrepurchasemin_JPN.dta
shell ren testrepurchasemin_tsxmin.dta testrepurchasemin_CAN.dta
shell ren testrepurchasemin_ftsemin.dta testrepurchasemin_GBR.dta
foreach h in USA JPN CAN GBR {
use testrepurchasemin_`h'.dta, clear
gen location="`h'"
rename date time 
save, replace

}


 //**********preparing VAR database*******// 
// ********  data with average repurchase*************////
clear all
#delimit;
cd "C:\d\FOUED\foued\travaux\habilitation\growh and payout";
#delimit cr  
set more off 
foreach i in CAN USA GBR JPN {
	use testrepurchaseAVE_`i', clear
	merge 1:1 time location using "C:\d\FOUED\foued\travaux\habilitation\growh and payout\data industrial production.dta"
	keep if _merge==3
	drop _merge
	save DATAVAR_AVE_`i', replace
	clear
	use "C:\d\FOUED\foued\travaux\habilitation\growh and payout\data index.dta", clear
    keep time var_gllogFTSEALLSHARE var_gllogSP500COMPOSITE var_llogSPTSXCOMPOSITEINDEX var_gllogTOPIX 
rename var_gllogFTSEALLSHARE var_GBR
rename var_gllogSP500COMPOSITE var_USA
rename var_llogSPTSXCOMPOSITEINDEX var_CAN
rename var_gllogTOPIX var_JPN
save test, replace
keep time var_`i'
save test`i', replace
merge 1:1 time using "C:\d\FOUED\foued\travaux\habilitation\growh and payout\DATAVAR_AVE_`i'.dta"
drop _merge
save DATAVAR_AVE_`i', replace
erase test`i'.dta
erase test.dta
	}
clear
 //**********preparing VAR database*******// 
// ********  data with min repurchase*************////
clear all
#delimit;
cd "C:\d\FOUED\foued\travaux\habilitation\growh and payout";
#delimit cr  
set more off 
foreach i in CAN USA GBR JPN {
	use testrepurchaseMIN_`i', clear
	merge 1:1 time location using "C:\d\FOUED\foued\travaux\habilitation\growh and payout\data industrial production.dta"
	keep if _merge==3
	drop _merge
	save DATAVAR_MIN_`i', replace
	clear
	use "C:\d\FOUED\foued\travaux\habilitation\growh and payout\data index.dta", clear
    keep time var_gllogFTSEALLSHARE var_gllogSP500COMPOSITE var_llogSPTSXCOMPOSITEINDEX var_gllogTOPIX 
rename var_gllogFTSEALLSHARE var_GBR
rename var_gllogSP500COMPOSITE var_USA
rename var_llogSPTSXCOMPOSITEINDEX var_CAN
rename var_gllogTOPIX var_JPN
save test, replace
keep time var_`i'
save test`i', replace
merge 1:1 time using "C:\d\FOUED\foued\travaux\habilitation\growh and payout\DATAVAR_MIN_`i'.dta"
drop _merge
save DATAVAR_MIN_`i', replace
erase test`i'.dta
erase test.dta
	}
clear
// ****************test hyppthesis**********************//
//******************************************************//
// ***********pretest*****************
varsoc repurchaseAVE var_JPN, maxlag(9)
// *************VAR model*****************
var repurchaseAVE var_JPN, lags(1/1) small
// ************white test***********************
varhet 
//*************JB test**************************
varnorm, jbera
//**************wald test************************
varwle
//***************LM test*************************
varlmar, mlag(1)
//**************stability test********************
varstable
//**************
//*********matrix calculation**********************
st_view(y=.,.," repurchaseAVE")
st_view(x=.,.,("const","repurchaseAVE","llrepurchaseAVE","gINDP"))
b=invsym(x'x)*x'y
e=y-x*b
getmata e    //(in stata code)
// ********predict residual****************//
reg repurchaseAVE lrepurchaseAVE llrepurchaseAVE lllrepurchaseAVE ///
llllrepurchaseAVE lgINDP llgINDP lllgINDP llllgINDP lvar_CAN llvar_CAN ///
lllvar_CAN llllvar_CAN
predict e, residuals

//************VAR causality***************************
************************************************
varsoc repurchaseAVE var_CAN  gINDP, maxlag(3)

//............generate lag variable..................//
gen lvar_CAN=l.var_CAN
gen llrepurchaseAVE=l2.repurchaseAVE



















	
	
	
	
	

