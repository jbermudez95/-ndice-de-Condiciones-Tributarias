/*
Nombre: 	 forecasting_ict_tests.do
Descripción: Do file para realizar análisis de pronóstico para el documento de 
			 trabajo "Estimación de un Índice de Condiciones Tributarias de 
			 Honduras".
Autor:    	 Jose Carlo Bermúdez
Afiliación:  Servicio de Administración de Rentas, Departamento de Estudios 
			 Fiscales y Económicos
*/

clear all
clear matrix

global path "C:\Users\Owner\Desktop\2021\Notas técnicas y papers\Indicador de recaudación\estimaciones\isre"
global out "C:\Users\Owner\Desktop\2021\Notas técnicas y papers\Indicador de recaudación\estimaciones"
cd "$path"

import excel "$path\base_fcst.xlsx", sheet("Sheet1") firstrow
drop A date

tsmktim date1, start(2008m1)
tsset date1

/*
Se comparan 3 modelos uniecuacionales:
Modelo 1 (m1). Tax_t = b0 + b1 * ICT_(t-i) + u_t
Modelo 2 (m2). Tax_t = a0 + a1 * Tax_(t-1) + v_t
Modelo 3 (m3). Tax_t = c0 + c1 * Tax_(t-i) + c2 * mwh_(t-i) + c3 * imae_(t-i) + e_t
*/

**********************************
*******      MODELO 1	   *******	
**********************************

ardl itrib ict, aic dots    								       //Sugiere un ARDL(4,4)
ardl itrib ict, ec1 lags(4 4) regstore(ardl_ec)
estat ectest												       //Cointegración a la Pesaran

qui reg itrib L(1/4).itrib L(1/4).ict, vce(robust)
predict itrib_m1_h1												   //h=1

ardl itrib ict if date1<=tm(2020m9), lags(4 4) regstore(ardl_1_h3) // h=3
	estimates restore ardl_1_h3
	`e(cmdline)' vce(robust)
	estimates store ardl_m1_h3
	forecast create model_m1_h3, replace
	forecast estimates ardl_m1_h3
	forecast solve, suffix(_m1_h3) begin(tm(2020m10)) log(off)
	
ardl itrib ict if date1<=tm(2020m6), lags(4 4) regstore(ardl_1_h6) // h=6
	estimates restore ardl_1_h6
	`e(cmdline)' vce(robust)
	estimates store ardl_m1_h6
	forecast create model_m1_h6, replace
	forecast estimates ardl_m1_h6
	forecast solve, suffix(_m1_h6) begin(tm(2020m7)) log(off)
	
ardl itrib ict if date1<=tm(2019m12), lags(4 4) regstore(ardl_1_h12) // h=12
	estimates restore ardl_1_h12
	`e(cmdline)' vce(robust)
	estimates store ardl_m1_h12
	forecast create model_m1_h12, replace
	forecast estimates ardl_m1_h12 
	forecast solve, suffix(_m1_h12) begin(tm(2020m1)) log(off)
	
	foreach var of varlist itrib_*_* {
		fcstats itrib `var'														
	}

**********************************
*******      MODELO 2	   *******	
**********************************

qui arima itrib, ar(1) vce(robust) 									// h =1
predict itrib_m2_h1 

qui arima itrib if date1<=tm(2020m9), ar(1) vce(robust) 			// h=3				
	estimates store ar_3h
	forecast create ar_model_3h, replace
	forecast estimates ar_3h
	forecast solve, suffix(_m2_h3) begin(tm(2020m10)) log(off)
	
qui arima itrib if date1<=tm(2020m6), ar(1) vce(robust) 	        // h=6 
	estimates store ar_6h
	forecast create ar_model_6h, replace
	forecast estimates ar_6h
	forecast solve, suffix(_m2_h6) begin(tm(2020m7)) log(off)
	
qui arima itrib if date1<=tm(2019m12), ar(1) vce(robust) 	        // h=12
	estimates store ar_12h
	forecast create ar_model_12h, replace
	forecast estimates ar_12h
	forecast solve, suffix(_m2_h12) begin(tm(2020m1)) log(off)

	foreach var of varlist itrib_*_* {
		fcstats itrib `var'														
	}

**********************************
*******      MODELO 3	   *******	
**********************************

qui reg itrib L(1/3).itrib L(1/3).mwh L(1/2).imae, vce(robust)					// h=1
predict itrib_m3_h1 

ardl itrib mwh imae if date1<=tm(2020m9), lags(3 3 2) regstore(ardl_3_3_2) 		// h=3
	estimates restore ardl_3_3_2
	`e(cmdline)' vce(robust)
	estimates store ardl_6
	forecast create ardl_6_model_3h, replace
	forecast estimates ardl_6
	forecast solve, suffix(_m3_h3) begin(tm(2020m10)) log(off)
	
ardl itrib mwh imae if date1<=tm(2020m6), lags(3 3 2) regstore(ardl_3_3_2_) 	// h=6
	estimates restore ardl_3_3_2_
	`e(cmdline)' vce(robust)
	estimates store ardl_6h
	forecast create ardl_6_model_6h, replace
	forecast estimates ardl_6h
	forecast solve, suffix(_m3_h6) begin(tm(2020m7)) log(off)

ardl itrib mwh imae if date1<=tm(2019m12), lags(3 3 2) regstore(ardl1_3_3_2_) 	// h=12
	estimates restore ardl1_3_3_2_
	`e(cmdline)' vce(robust)
	estimates store ardl_12h
	forecast create ardl_model_12h, replace
	forecast estimates ardl_12h
	forecast solve, suffix(_m3_h12) begin(tm(2020m1)) log(off)
	
	foreach var of varlist itrib_*_* {
		fcstats itrib `var'														
	}
	

**********************************
****     DIEBOLD Y MARIANO    ****	
**********************************

*Capacidad predictiva para h=1

dmariano itrib itrib_m1_h1 itrib_m2_h1, crit(MSE)
dmariano itrib itrib_m1_h1 itrib_m3_h1, crit(MSE)
dmariano itrib itrib_m2_h1 itrib_m3_h1, crit(MSE)

*Capacidad predictiva para h=3

dmariano itrib itrib_m1_h3 itrib_m2_h3, crit(MSE)
dmariano itrib itrib_m1_h3 itrib_m3_h3, crit(MSE)
dmariano itrib itrib_m2_h3 itrib_m3_h3, crit(MSE)

*Capacidad predictiva para h=6

dmariano itrib itrib_m1_h6 itrib_m2_h6, crit(MSE)
dmariano itrib itrib_m1_h6 itrib_m3_h6, crit(MSE)
dmariano itrib itrib_m2_h6 itrib_m3_h6, crit(MSE)

*Capacidad predictiva para h=12

dmariano itrib itrib_m1_h12 itrib_m2_h12, crit(MSE)
dmariano itrib itrib_m1_h12 itrib_m3_h12, crit(MSE)
dmariano itrib itrib_m2_h12 itrib_m3_h12, crit(MSE)

