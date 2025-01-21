**open dataset**

cd "/Users/biancacarducci/Desktop/FSCI performance analyses"
import delimited "Master Files/FSCI_2024.csv", clear

**changing format of values**

rename value valuestring
generate value=real(valuestring)
list value valuestring

**select indicator**
log using "Access to Safe Water/Safe Water.log", replace
keep if indicator == "% population using safely managed drinking water services (SDG 6.1.1)"
tab year
label variable uncontinentalregion "UN Continental Region"
label variable year "Year"
label variable income_group "World Bank Country Income"
label variable country "Country"
label variable value "Value"

**data cleaning and creating last year by country**

drop if valuestring == "NA"
bysort country: gen num = _n
bysort country: gen numT = _N
tab numT if num==1
list country year value numT
codebook country year
bysort country:egen maxyear=max(year)
tab maxyear
sort country year
list country year value maxyear if maxyear==year
codebook country if year==maxyear

bysort country:egen minyear=min(year)
tab minyear
sort country year
list country year value minyear if minyear==year
codebook country if year==minyear

**MILESTONE ANALYSES GLOBAL**

**find milestone globally - top 20% using latest data point**

summarize value if year==maxyear, detail
centile value if maxyear==year, centile(90 85 80)
egen c20 = pctile(value) if maxyear==year, p(80)
label variable c20 "Milestone Value"

**#DISTANCE TO MILESTONE

**distance to milestone from latest data point**

generate projectyear=(value-c20)
label variable projectyear "Distance to Milestone"
sort country year
list country year value c20 projectyear if maxyear==year
summarize projectyear if maxyear==year, detail
tabstat projectyear, by(uncontinentalregion) statistics(mean)
tabout country if maxyear==year using "Access to Safe Water/Milestone/Distance/Global/safewaterdistancetable.txt",rep c(mean year mean projectyear) f(0 2) clab(Latest_Year Distance_to_Milestone) sum

export excel country uncontinentalregion income_group year c20 projectyear gdp if maxyear==year using "Access to Safe Water/Milestone/Distance/Global/safewaterdistancetable.xls", firstrow(varlabels) replace

encode country, gen(countryN)

encode gdp, gen(gdpN)

twoway (scatter projectyear gdpN if maxyear==year & uncontinentalregion == "Africa", jitter(200) mlabel(iso3) mlabsize(tiny) mcolor(blue)) ///
       (scatter projectyear gdpN if maxyear==year & uncontinentalregion == "Asia", jitter(200) mlabel(iso3) mlabsize(tiny) mcolor(red)) ///
       (scatter projectyear gdpN if maxyear==year & uncontinentalregion == "Americas", jitter(200) mlabel(iso3) mlabsize(tiny) mcolor(green)) ///
       (scatter projectyear gdpN if maxyear==year & uncontinentalregion == "Europe", jitter(200) mlabel(iso3) mlabsize(tiny) mcolor(orange)) ///
       (scatter projectyear gdpN if maxyear==year & uncontinentalregion == "Oceania", jitter(200) mlabel(iso3) mlabsize(tiny) mcolor(purple)) ///
       , title("Distance from Global Milestone, Access to Safe Water") ///
         ytitle("Distance from Global Milestone (% of population)") ///
         xtitle("GDP") ///
         xtick(1(1)200) ///
         xscale(range(1 200)) ///
         yscale(range(0 50)) ///
         legend(order(1 "Africa" 2 "Asia" 3 "Americas" 4 "Europe" 5 "Oceania"))

		 
graph export "Access to Safe Water/Milestone/Distance/Global/safewaterdistanceall.png", as(png) replace
		 
graph export "Fruit Availability/Milestone/Distance/Global/fruitavailabilitydistanceall.png", as(png) replace 
twoway (scatter projectyear countryN if maxyear==year & uncontinentalregion == "Africa", jitter(200) mlabel(iso3) mlabsize(tiny) title( "Distance from Global Milestone, Africa") ytitle("Distance from Global Milestone (% of population)") xtitle(Country) xlabel(, noticks nolabels) xscale(range(1 200)) yscale(range(90 0)))
graph export "Access to Safe Water/Milestone/Distance/Global/safewaterdistanceafrica.png", as(png) replace
graph save "Access to Safe Water/Milestone/Distance/Global/safewaterdistanceafrica.gph", replace

twoway (scatter projectyear countryN if maxyear==year & uncontinentalregion == "Americas", jitter(200) mlabel(iso3) mlabsize(tiny) title( "Distance from Global Milestone, Americas") ytitle("Distance from Global Milestone (% of population)") xtitle(Country) xlabel(, noticks nolabels) xscale(range(1 200)) yscale(range(90 0)))
graph export "Access to Safe Water/Milestone/Distance/Global/safewaterdistanceamericas.png", as(png) replace
graph save "Access to Safe Water/Milestone/Distance/Global/safewaterdistanceamericas.gph", replace

twoway (scatter projectyear countryN if maxyear==year & uncontinentalregion == "Asia", jitter(200) mlabel(iso3) mlabsize(tiny) title( "Distance from Global Milestone, Asia") ytitle("Distance from Global Milestone (% of population)") xtitle(Country) xlabel(, noticks nolabels) xscale(range(1 200)) yscale(range(90 0)))
graph export "Access to Safe Water/Milestone/Distance/Global/safewaterdistanceamericas.png", as(png) replace
graph save "Access to Safe Water/Milestone/Distance/Global/safewaterdistanceasia.gph", replace

twoway (scatter projectyear countryN if maxyear==year & uncontinentalregion == "Europe", jitter(200) mlabel(iso3) mlabsize(tiny) title( "Distance from Global Milestone, Europe") ytitle("Distance from Global Milestone (% of population)") xtitle(Country) xlabel(, noticks nolabels) xscale(range(1 200)) yscale(range(20 0)))
graph export "Access to Safe Water/Milestone/Distance/Global/safewaterdistanceeurope.png", as(png) replace
graph save "Access to Safe Water/Milestone/Distance/Global/safewaterdistanceeurope.gph", replace

twoway (scatter projectyear countryN if maxyear==year & uncontinentalregion == "Oceania", jitter(200) mlabel(iso3) mlabsize(tiny) title( "Distance from Global Milestone, Oceania") ytitle("Distance from Global Milestone (% of population)") xtitle(Country) xlabel(, noticks nolabels) xscale(range(1 200)) yscale(range(20 0)))
graph export "Access to Safe Water/Milestone/Distance/Global/safewaterdistanceoceania.png", as(png) replace
graph save "Access to Safe Water/Milestone/Distance/Global/safewaterdistanceoceania.gph", replace

**#VELOCITY MILESTONE GLOBAL

**rate of change required from latest data point to milestone by 2030 globally, CAGR**

generate predrate=(c20/value)^(1/(2030-year))-1 if maxyear==year
label variable predrate "Velocity to Global Milestone, Latest Value, CAGR"
sort country year 
list country year value c20 projectyear predrate, sepby (country)


**rate of change required from latest data point to milestone by 2030 globally, linear**

generate predratelin=(c20-value)/(2030-year) if maxyear==year
label variable predratelin "Distance to Global Milestone, Latest Value, Linear"
sort country year 
list country year value c20 projectyear predrate predratelin, sepby (country)


**3-year average rate of change required from latest data point to milestone by 2030 globally, CAGR**

sort country year
by country: egen averate = mean(value) if maxyear-year<3
label variable averate "Average"
generate predrate3=(c20/averate)^(1/(2030-year))-1 if maxyear==year
label variable predrate3 "Distance to Global Milestone, Average, CAGR"
sort country year 
list country year value c20 projectyear predrate predratelin averate predrate3, sepby (country)

**3-year average rate of change required from latest data point to milestone by 2030 globally, linear**

generate predratelin3=(c20-averate)/(2030-year) if maxyear==year
label variable predratelin3 "Distance to Global Milestone, Average, Linear"
sort country year 
list country year value c20 projectyear predrate predratelin averate predrate3 predratelin3, sepby (country)

**rate of change required predicted linear rate using trend**
*sort country year
*by country: gen numt = _N
*mixed value year || country: year
*predict ratelin*, reffects
*list country year value c20 projectyear predrate predratelin averate predrate3 predratelin3 ratelin1, sepby (country)

tabout country uncontinentalregion income_group year value c20 projectyear predrate predratelin averate predrate3 predratelin3 if maxyear==year using "Access to Safe Water/Milestone/Velocity/Global/safewaterallvelocity.txt",rep c(mean year mean value mean c20 mean projectyear mean predrate mean predratelin mean averate mean predrate3 mean predratelin3) f(0 2) clab(Latest_Year Latest_Value Global_Milestone_Value Distance_to_Global_Milestone Velocity_to_Global_Milestone_CAGR Velocity_to_Global_Milestone_Linear Averate_Rate Velocity_to_Global_Milestone_CAGR(AveRate) Velocity_to_Global_Milestone_Linear(AveRate)) sum

export excel country uncontinentalregion income_group year value c20 projectyear predrate predratelin averate predrate3 predratelin3 if maxyear==year using "Access to Safe Water/Milestone/Velocity/Global/safewaterallvelocity.xls", firstrow(varlabels) replace

pwcorr predrate predratelin predrate3 predratelin3, sig

**TARGET ANALYSES**


**#DISTANCE TO TARGET

generate targetvalue = 100 if indicator == "% population using safely managed drinking water services (SDG 6.1.1)"
label variable targetvalue "Target Value"
generate projecttargetyear=(value-targetvalue)
label variable projecttargetyear "Distance to Target"
sort country year
tabstat projecttargetyear, by(uncontinentalregion) statistics(mean)
tabout country if maxyear==year using "Access to Safe Water/Target/Distance/safewaterdistancetarget.txt",rep c(mean year mean projecttargetyear) f(0 2) clab(Latest_Year Distance_to_Target) sum

export excel country uncontinentalregion income_group year targetvalue projecttargetyear gdp if maxyear==year using "Access to Safe Water/Target/Distance/safewaterdistancetarget.xls", firstrow(varlabels) replace

twoway (scatter projecttargetyear gdpN if maxyear==year & uncontinentalregion == "Africa", jitter(200) mlabel(iso3) mlabsize(tiny) mcolor(blue)) ///
       (scatter projecttargetyear gdpN if maxyear==year & uncontinentalregion == "Asia", jitter(200) mlabel(iso3) mlabsize(tiny) mcolor(red)) ///
       (scatter projecttargetyear gdpN if maxyear==year & uncontinentalregion == "Americas", jitter(200) mlabel(iso3) mlabsize(tiny) mcolor(green)) ///
       (scatter projecttargetyear gdpN if maxyear==year & uncontinentalregion == "Europe", jitter(200) mlabel(iso3) mlabsize(tiny) mcolor(orange)) ///
       (scatter projecttargetyear gdpN if maxyear==year & uncontinentalregion == "Oceania", jitter(200) mlabel(iso3) mlabsize(tiny) mcolor(purple)) ///
       , title("Distance from Global Target, Access to Safe Water") ///
         ytitle("Distance from Global Target (% of population)") ///
         xtitle("GDP") ///
         xtick(1(1)200) ///
         xscale(range(1 200)) ///
         yscale(range(0 50)) ///
         legend(order(1 "Africa" 2 "Asia" 3 "Americas" 4 "Europe" 5 "Oceania"))
		 
graph export "Access to Safe Water/Target/Distance/safewaterdistanceall.png", as(png) replace

twoway (scatter projecttargetyear countryN if maxyear==year & uncontinentalregion == "Africa", jitter(200) mlabel(iso3) mlabsize(tiny) title( "Distance from Target, Africa") ytitle("Distance from Target (% of population)") xtitle(Country) xlabel(, noticks nolabels) xscale(range(1 200)) yscale(range(90 0)))
graph export "Access to Safe Water/Target/Distance/safewaterdistanceafrica.png", as(png) replace

twoway (scatter projecttargetyear countryN if maxyear==year & uncontinentalregion == "Americas", jitter(200) mlabel(iso3) mlabsize(tiny) title( "Distance from Target, Americas") ytitle("Distance from Target (% of population)") xtitle(Country) xlabel(, noticks nolabels) xscale(range(1 200)) yscale(range(90 0)))
graph export "Access to Safe Water/Target/Distance/safewaterdistanceamericas.png", as(png) replace

twoway (scatter projecttargetyear countryN if maxyear==year & uncontinentalregion == "Asia", jitter(200) mlabel(iso3) mlabsize(tiny) title( "Distance from Target, Asia") ytitle("Distance from Target (% of population)") xtitle(Country) xlabel(, noticks nolabels) xscale(range(1 200)) yscale(range(90 0)))
graph export "Access to Safe Water/Target/Distance/safewaterdistanceasia.png", as(png) replace

twoway (scatter projecttargetyear countryN if maxyear==year & uncontinentalregion == "Europe", jitter(200) mlabel(iso3) mlabsize(tiny) title( "Distance from Target, Europe") ytitle("Distance from Target (% of population)") xtitle(Country) xlabel(, noticks nolabels) xscale(range(1 200)) yscale(range(20 0)))
graph export "Access to Safe Water/Target/Distance/safewaterdistanceeurope.png", as(png) replace

twoway (scatter projecttargetyear countryN if maxyear==year & uncontinentalregion == "Oceania", jitter(200) mlabel(iso3) mlabsize(tiny) title( "Distance from Target, Oceania") ytitle("Distance from Target (% of population)") xtitle(Country) xlabel(, noticks nolabels) xscale(range(1 200)) yscale(range(20 0)))
graph export "Access to Safe Water/Target/Distance/safewaterdistanceoceania.png", as(png) replace


**#VELOCITY TARGET

**rate of change required from latest data point to target by 2030 globally, CAGR**

generate predratetarget=(targetvalue/value)^(1/(2030-year))-1 if maxyear==year
label variable predratetarget "Velocity to Global Target, Latest Value, CAGR"
sort country year 
list country year value targetvalue projecttargetyear predratetarget, sepby (country)

**rate of change required from latest data point to target by 2030 globally, linear**

generate predratetargetlin=(targetvalue-value)/(2030-year) if maxyear==year
label variable predratetargetlin "Velocity to Global Target, Latest Value, Linear"
sort country year 
list country year value targetvalue projecttargetyear predratetarget predratetargetlin, sepby (country)

**3-year average rate of change required from latest data point to target by 2030 globally, CAGR**

sort country year
generate predratetarget3=(targetvalue/averate)^(1/(2030-year))-1 if maxyear==year
label variable predratetarget3 "Velocity to Global Target, Average, CAGR"
sort country year 
list country year value targetvalue averate projecttargetyear predratetarget predratelin predratetarget3, sepby (country)

**3-year average rate of change required from latest data point to target by 2030 globally, linear**

generate predratetargetlin3=(targetvalue-averate)/(2030-year) if maxyear==year
label variable predratetargetlin3 "Velocity to Global Target, Average, Linear"
sort country year 
list country year value targetvalue averate projecttargetyear predratetarget predratetargetlin predratetarget3 predratetargetlin3, sepby (country)

**rate of change required predicted linear rate using trend**
*sort country year
*by country: gen numt = _N
*mixed value year || country: year
*predict ratelin*, reffects
*list country year value targetvalue averate projecttargetyear predratetarget predratetargetlin predratetarget3 predratetargetlin3 ratelin1, sepby (country)

tabout country uncontinentalregion income_group year value targetvalue projecttargetyear predratetarget predratetargetlin averate predratetarget3 predratetargetlin3 if maxyear==year using "Access to Safe Water/Target/Velocity/safewaterallvelocity.txt",rep c(mean year mean value mean targetvalue mean projecttargetyear mean predratetarget mean predratetargetlin mean averate mean predratetarget3 mean predratetargetlin3) f(0 2) clab(Latest_Year Latest_Value Global_Target_Value Distance_to_Global_Target Velocity_to_Global_Target_CAGR Velocity_to_Global_Target_Linear Averate_Rate Velocity_to_Global_Target_CAGR(AveRate) Velocity_to_Global_Target_Linear(AveRate)) sum

export excel country uncontinentalregion income_group year value targetvalue projecttargetyear predratetarget predratetargetlin averate predratetarget3 predratetargetlin3 if maxyear==year using "Access to Safe Water/Target/Velocity/safewaterallvelocity.xls", firstrow(varlabels) replace

pwcorr predratetarget predratetargetlin predratetarget3 predratetargetlin3, sig


**MILESTONE ANALYSES REGIONAL**

**#DISTANCE TO MILESTONE REGIONAL

**find milestone regionally - top 20% using latest data point**

bysort uncontinentalregion:egen creg20 = pctile(value) if maxyear==year, p(80)
label variable creg20 "Regional Milestone Value"
generate projectregyear=(value-creg20) if maxyear==year
label variable projectregyear "Distance to Regional Milestone"
sort country year
summarize projectregyear if maxyear==year, detail
bysort uncontinentalregion:list country year value creg20 projectregyear if maxyear==year

tabout country if maxyear==year using "Access to Safe Water/Milestone/Distance/Regional/safewaterdistanceregmilestone.txt",rep c(mean year mean projectregyear) f(0 2) clab(Latest_Year Distance_to_Regional_Milestone) sum

export excel country uncontinentalregion income_group year creg20 projectregyear gdp if maxyear==year using "Access to Safe Water/Milestone/Distance/Regional/safewaterdistanceregmilestone.xls", firstrow(varlabels) replace

twoway (scatter projectregyear gdpN if maxyear==year & uncontinentalregion == "Africa", jitter(200) mlabel(iso3) mlabsize(tiny) mcolor(blue)) ///
       (scatter projectregyear gdpN if maxyear==year & uncontinentalregion == "Asia", jitter(200) mlabel(iso3) mlabsize(tiny) mcolor(red)) ///
       (scatter projectregyear gdpN if maxyear==year & uncontinentalregion == "Americas", jitter(200) mlabel(iso3) mlabsize(tiny) mcolor(green)) ///
       (scatter projectregyear gdpN if maxyear==year & uncontinentalregion == "Europe", jitter(200) mlabel(iso3) mlabsize(tiny) mcolor(orange)) ///
       (scatter projectregyear gdpN if maxyear==year & uncontinentalregion == "Oceania", jitter(200) mlabel(iso3) mlabsize(tiny) mcolor(purple)) ///
       , title("Distance from Regional Milestone, Access to Safe Water") ///
         ytitle("Distance from Regional Milestone (% of population)") ///
         xtitle("GDP") ///
         xtick(1(1)200) ///
         xscale(range(1 200)) ///
         yscale(range(0 50)) ///
         legend(order(1 "Africa" 2 "Asia" 3 "Americas" 4 "Europe" 5 "Oceania"))
		 
graph export "Access to Safe Water/Milestone/Distance/Regional/safewaterdistanceall.png", as(png) replace

twoway (scatter projectregyear countryN if maxyear==year & uncontinentalregion == "Africa", jitter(200) mlabel(iso3) mlabsize(tiny) title( "Distance from Regional Milestone, Africa") ytitle("Distance from Regional Milestone (% of population)") xtitle(Country) xlabel(, noticks nolabels) xscale(range(1 200)) yscale(range(90 0)))
graph export "Access to Safe Water/Milestone/Distance/Regional/safewaterdistanceafrica.png", as(png) replace
graph save "Access to Safe Water/Milestone/Distance/Regional/safewaterdistanceafrica.gph", replace

twoway (scatter projectregyear countryN if maxyear==year & uncontinentalregion == "Americas", jitter(200) mlabel(iso3) mlabsize(tiny) title( "Distance from Regional Milestone, Americas") ytitle("Distance from Regional Milestone (% of population)") xtitle(Country) xlabel(, noticks nolabels) xscale(range(1 200)) yscale(range(90 0)))
graph export "Access to Safe Water/Milestone/Distance/Regional/safewaterdistanceamericas.png", as(png) replace
graph save "Access to Safe Water/Milestone/Distance/Regional/safewaterdistanceamericas.gph", replace

twoway (scatter projectregyear countryN if maxyear==year & uncontinentalregion == "Asia", jitter(200) mlabel(iso3) mlabsize(tiny) title( "Distance from Regional Milestone, Asia") ytitle("Distance from Regional Milestone (% of population)") xtitle(Country) xlabel(, noticks nolabels) xscale(range(1 200)) yscale(range(90 0)))
graph export "Access to Safe Water/Milestone/Distance/Regional/safewaterdistanceamericas.png", as(png) replace
graph save "Access to Safe Water/Milestone/Distance/Regional/safewaterdistanceasia.gph", replace

twoway (scatter projectregyear countryN if maxyear==year & uncontinentalregion == "Europe", jitter(200) mlabel(iso3) mlabsize(tiny) title( "Distance from Regional Milestone, Europe") ytitle("Distance from Regional Milestone (% of population)") xtitle(Country) xlabel(, noticks nolabels) xscale(range(1 200)) yscale(range(20 0)))
graph export "Access to Safe Water/Milestone/Distance/Regional/safewaterdistanceeurope.png", as(png) replace
graph save "Access to Safe Water/Milestone/Distance/Regional/safewaterdistanceeurope.gph", replace

twoway (scatter projectregyear countryN if maxyear==year & uncontinentalregion == "Oceania", jitter(200) mlabel(iso3) mlabsize(tiny) title( "Distance from Regional Milestone, Oceania") ytitle("Distance from Regional Milestone (% of population)") xtitle(Country) xlabel(, noticks nolabels) xscale(range(1 200)) yscale(range(20 0)))
graph export "Access to Safe Water/Milestone/Distance/Regional/safewaterdistanceoceania.png", as(png) replace
graph save "Access to Safe Water/Milestone/Distance/Regional/safewaterdistanceoceania.gph", replace


**#VELOCITY MILESTONE REGIONAL

**rate of change required from latest data point to milestone by 2030 regionally, CAGR**

generate predratereg=(creg20/value)^(1/(2030-year))-1 if maxyear==year
label variable predratereg "Velocity to Regional Milestone, Latest Value, CAGR"
sort country year 
bysort uncontinentalregion:list country year value projectregyear creg20 predratereg, sepby (country)

**rate of change required from latest data point to milestone by 2030 regionally, linear**

generate predratelinreg=(creg20-value)/(2030-year) if maxyear==year
label variable predratelinreg "Velocity to Regional Milestone, Latest Value, Linear"
sort country year 
bysort uncontinentalregion:list country year value projectregyear c20 predratereg predratelinreg, sepby (country)

**3-year average rate of change required from latest data point to milestone by 2030 globally, CAGR**

sort country year
generate predratereg3=(creg20/averate)^(1/(2030-year))-1 if maxyear==year
label variable predratereg3 "Velocity to Regional Milestone, Average, CAGR"
sort country year 
bysort uncontinentalregion:list country year value averate projectregyear creg20 predratereg predratereg3, sepby (country)

**3-year average rate of change required from latest data point to milestone by 2030 globally, linear**

generate predratelinreg3=(creg20-averate)/(2030-year) if maxyear==year
label variable predratelinreg3 "Velocity to Regional Milestone, Average, Linear"
sort country year 
bysort uncontinentalregion:list country year value averate projectregyear creg20 predratelinreg predratelinreg3, sepby (country)

tabout country uncontinentalregion income_group year value creg20 projectregyear predratereg predratelinreg averate predratereg3 predratelinreg3 if maxyear==year using "Access to Safe Water/Milestone/Velocity/Regional/safewaterallvelocityreg.txt",rep c(mean year mean value mean creg20 mean projectregyear mean predratereg mean predratelinreg mean averate mean predratereg3 mean predratelinreg3) f(0 2) clab(Latest_Year Latest_Value Regional_Milestone_Value Distance_to_Regional_Milestone Velocity_to_Regional_Milestone_CAGR Velocity_to_Regional_Milestone_Linear Averate_Rate Velocity_to_Regional_Milestone_CAGR(AveRate) Velocity_to_Regional_Milestone_Linear(AveRate)) sum

export excel country uncontinentalregion income_group year value creg20 projectregyear predratereg predratelinreg averate predratereg3 predratelinreg3 if maxyear==year using "Access to Safe Water/Milestone/Velocity/Regional/safewaterallvelocityreg.xls", firstrow(varlabels) replace

pwcorr predratereg predratelinreg predratereg3 predratelinreg3, sig

**#DISTANCE TO MILESTONE INCOME

**find milestone - top 20% by country income group**

bysort income_group:egen cinc20 = pctile(value) if maxyear==year, p(80)
label variable cinc20 "Income Milestone Value"
generate projectincyear=(value-cinc20) if maxyear==year
label variable projectincyear "Distance to Income Milestone"
sort country year
summarize projectincyear if maxyear==year, detail
bysort income_group:list country year value cinc20 projectincyear if maxyear==year

tabout country if maxyear==year using "Access to Safe Water/Milestone/Distance/Income/safewaterdistanceincmilestone.txt",rep c(mean year mean projectincyear) f(0 2) clab(Latest_Year Distance_to_Income_Milestone) sum

export excel country uncontinentalregion income_group year cinc20 projectincyear gdp if maxyear==year using "Access to Safe Water/Milestone/Distance/Income/safewaterdistanceincmilestone.xls", firstrow(varlabels) replace

export excel country uncontinentalregion income_group year creg20 projectregyear gdp if maxyear==year using "Access to Safe Water/Milestone/Distance/Regional/safewaterdistanceregmilestone.xls", firstrow(varlabels) replace

twoway (scatter projectincyear gdpN if maxyear==year & uncontinentalregion == "Africa", jitter(200) mlabel(iso3) mlabsize(tiny) mcolor(blue)) ///
       (scatter projectincyear gdpN if maxyear==year & uncontinentalregion == "Asia", jitter(200) mlabel(iso3) mlabsize(tiny) mcolor(red)) ///
       (scatter projectincyear gdpN if maxyear==year & uncontinentalregion == "Americas", jitter(200) mlabel(iso3) mlabsize(tiny) mcolor(green)) ///
       (scatter projectincyear gdpN if maxyear==year & uncontinentalregion == "Europe", jitter(200) mlabel(iso3) mlabsize(tiny) mcolor(orange)) ///
       (scatter projectincyear gdpN if maxyear==year & uncontinentalregion == "Oceania", jitter(200) mlabel(iso3) mlabsize(tiny) mcolor(purple)) ///
       , title("Distance from Income Milestone, Access to Safe Water") ///
         ytitle("Distance from Income Milestone (% of population)") ///
         xtitle("GDP") ///
         xtick(1(1)200) ///
         xscale(range(1 200)) ///
         yscale(range(0 50)) ///
         legend(order(1 "Africa" 2 "Asia" 3 "Americas" 4 "Europe" 5 "Oceania"))
		 
graph export "Access to Safe Water/Milestone/Distance/Income/safewaterdistanceall.png", as(png) replace

twoway (scatter projectincyear countryN if maxyear==year & income_group == "High income", jitter(200) mlabel(iso3) mlabsize(tiny) title( "Distance from Income Milestone, High income") ytitle("Distance from Income Milestone (% of population)") xtitle(Country) xlabel(, noticks nolabels) xscale(range(1 200)) yscale(range(90 0)))
graph export "Access to Safe Water/Milestone/Distance/Income/safewaterdistancehighinc.png", as(png) replace
graph save "Access to Safe Water/Milestone/Distance/Income/safewaterdistancehighinc.gph", replace

twoway (scatter projectincyear countryN if maxyear==year & income_group == "Upper middle income", jitter(200) mlabel(iso3) mlabsize(tiny) title( "Distance from Income Milestone, Upper middle income") ytitle("Distance from Income Milestone (% of population)") xtitle(Country) xlabel(, noticks nolabels) xscale(range(1 200)) yscale(range(90 0)))
graph export "Access to Safe Water/Milestone/Distance/Income/safewaterdistanceuppermid.png", as(png) replace
graph save "Access to Safe Water/Milestone/Distance/Income/safewaterdistanceuppermid.gph", replace

twoway (scatter projectincyear countryN if maxyear==year & income_group == "Lower middle income", jitter(200) mlabel(iso3) mlabsize(tiny) title( "Distance from Income Milestone, Lower middle income") ytitle("Distance from Income Milestone (% of population)") xtitle(Country) xlabel(, noticks nolabels) xscale(range(1 200)) yscale(range(90 0)))
graph export "Access to Safe Water/Milestone/Distance/Income/safewaterdistancelowermid.png", as(png) replace
graph save "Access to Safe Water/Milestone/Distance/Income/safewaterdistancelowermid.gph", replace

twoway (scatter projectincyear countryN if maxyear==year & income_group == "Low income", jitter(200) mlabel(iso3) mlabsize(tiny) title( "Distance from Income Milestone, Low income") ytitle("Distance from Income Milestone (% of population)") xtitle(Country) xlabel(, noticks nolabels) xscale(range(1 200)) yscale(range(20 0)))
graph export "Access to Safe Water/Milestone/Distance/Income/safewaterdistancelow.png", as(png) replace
graph save "Access to Safe Water/Milestone/Distance/Income/safewaterdistancelow.gph", replace


**#VELOCITY MILESTONE INCOME

**rate of change required from latest data point to milestone by 2030, CAGR**

generate predrateinc=(cinc20/value)^(1/(2030-year))-1 if maxyear==year
label variable predrateinc "Velocity to Income Milestone, Latest Value, CAGR"
sort country year 
list country year value projectincyear c20 predrateinc, sepby (country)

**rate of change required from latest data point to milestone by 2030, linear**

generate predratelininc=(cinc20-value)/(2030-year) if maxyear==year
label variable predratelininc "Velocity to Income Milestone, Latest Value, Linear"
sort country year 
list country year value projectincyear cinc20 predrateinc predratelininc, sepby (country)

**3-year average rate of change required from latest data point to milestone by 2030, CAGR**

sort country year
generate predrateinc3=(cinc20/averate)^(1/(2030-year))-1 if maxyear==year
label variable predrateinc3 "Velocity to Income Milestone, Average Rate, CAGR"
sort country year 
list country year value averate projectincyear cinc20 predrateinc predrateinc3, sepby (country)

**3-year average rate of change required from latest data point to milestone by 2030, linear**

generate predratelininc3=(cinc20-averate)/(2030-year) if maxyear==year
label variable predratelininc3 "Velocity to Income Milestone, Average Rate, Linear"
sort country year 
list country year value averate projectincyear cinc20 predratelininc predratelininc3, sepby (country)


tabout country uncontinentalregion income_group year value cinc20 projectincyear predrateinc predratelininc averate predrateinc3 predratelininc3 if maxyear==year using "Access to Safe Water/Milestone/Velocity/Income/safewaterallvelocityinc.txt",rep c(mean year mean value mean cinc20 mean projectincyear mean predrateinc mean predratelininc mean averate mean predrateinc3 mean predratelininc3) f(0 2) clab(Latest_Year Latest_Value Income_Milestone_Value Distance_to_Income_Milestone Velocity_to_Income_Milestone_CAGR Velocity_to_Income_Milestone_Linear Averate_Rate Velocity_to_Income_Milestone_CAGR(AveRate) Velocity_to_Income_Milestone_Linear(AveRate)) sum

export excel country uncontinentalregion income_group year value cinc20 projectincyear predrateinc predratelininc averate predrateinc3 predratelininc3 if maxyear==year using "Access to Safe Water/Milestone/Velocity/Income/safewaterallvelocityinc.xls", firstrow(varlabels) replace


pwcorr predrateinc predratelininc predrateinc3 predratelininc3, sig


**#OVERALL SUMMARY**
preserve
collapse creg20, by (uncontinentalregion)
list
generate rid=1
encode uncontinentalregion, generate (region)
drop uncontinentalregion
codebook region
reshape wide creg20, i(rid) j(region)
save creg20.dta, replace
restore
preserve
collapse targetvalue c20 cinc20, by (income_group)
list
generate iid=1
encode income_group, generate (income)
drop if income_group=="NA"
drop income_group
codebook income
reshape wide cinc20, i(iid) j(income)
merge using creg20.dta
drop rid iid _merge
generate indicator = "% population using safely managed drinking water services (SDG 6.1.1)"
label var indicator "Indicator"
label var targetvalue "Global Target"
label var c20 "Global Milestone"
label var creg201 "Regional Milestone, Africa"
label var creg202 "Regional Milestone, Americas"
label var creg203 "Regional Milestone, Asia"
label var creg204 "Regional Milestone, Europe"
label var creg205 "Regional Milestone, Oceania"
label var cinc201 "Income Milestone, High Income"
label var cinc202 "Income Milestone, Low Income"
label var cinc203 "Income Milestone, Lower Middle Income"
label var cinc205 "Income Milestone, Upper Middle Income"
order indicator targetvalue c20 creg201-creg205 cinc201 cinc202 cinc203 cinc205
save "/Users/biancacarducci/Desktop/FSCI performance analyses/Access to Safe Water/Summary of Reference Points.dta", replace
export excel using "/Users/biancacarducci/Desktop/FSCI performance analyses/Access to Safe Water/Summary Reference Points.xlsx", firstrow(varlabels) replace
restore

**#AVERAGE DISTANCES BY REGION**
preserve
collapse projecttargetyear projectyear projectregyear projectincyear, by (uncontinentalregion)
generate indicator = "% population using safely managed drinking water services (SDG 6.1.1)"
label var indicator "Indicator"
label var projecttargetyear "Average Distance to Target"
label var projectyear "Average Distance to Milestone"
label var projectregyear "Average Distance to Regional Milestone"
label var projectincyear "Average Distance to Income Milestone"
list
order indicator uncontinentalregion projecttargetyear projectyear projectregyear projectincyear
save "/Users/biancacarducci/Desktop/FSCI performance analyses/Access to Safe Water/Summary Distances.dta", replace
export excel using "/Users/biancacarducci/Desktop/FSCI performance analyses/Access to Safe Water/Summary Distances.xlsx", firstrow(varlabels) replace
restore

**#EXPECTED VALUE AT 2030**

keep if year >= 2015 & year <= 2024
bysort country:egen maxyear2030=max(year)
bysort country:egen minyear2030=min(year)
generate timediff=(maxyear2030-minyear2030)
generate vt=value if year==maxyear2030
label variable vt "Latest Year"
generate vb=value if year==minyear2030
label variable vb "Baseline Year"
sort country vt
by country: carryforward vt, replace
sort country vb
by country: carryforward vb, replace
list country year value minyear2030 maxyear2030 timediff vt vb, sepby (country)
generate cagr=(vt/vb)^(1/timediff)-1
label variable cagr "Compound Annual Growth Rate"
generate v2030e=vt*(1+cagr)^(2030-maxyear)
label variable v2030e "Expected Value at 2030"
generate progress=(v2030e-vb)/(targetvalue-vb)
label variable progress "Progress"
generate progresscat=0 if progress<0
replace progresscat=1 if progress>=0 & progress<0.1
replace progresscat=2 if progress>=0.1 & progress<0.5
replace progresscat=3 if progress>=0.5 & progress<0.95
replace progresscat=4 if progress>=0.95 & progress!=. | vb>=targetvalue
label define progress 0 "Regression" 1 "Stagnation" 2 "Marginal progress" 3 "Moderate progress" 4 "On track/target met"
label values progresscat progress
sort country year
list country year value cagr progress progresscat, sepby (country)
tabulate progresscat if year==maxyear, missing
list country year value cagr progress progresscat if progresscat==., sepby (country)
export excel country year uncontinentalregion income_group value cagr progress progresscat using "Access to Safe Water/Progress/Progress Assessment.xls", firstrow(varlabels) replace

**#EXPECTED VALUE AT 2030, EACH YEAR, USING 2015 AS BASELINE**

keep if year >= 2015 & year <= 2024
bysort country:egen minyearind2030=min(year)
generate timediffind=(year-minyearind2030)
generate vtind=value
label variable vtind "Current Year"
generate vbind=value if year==minyearind2030
label variable vbind "Baseline Year"
sort country vbind
by country: carryforward vbind, replace
list country year value minyearind2030 timediffind vtind vbind, sepby (country)
generate cagrind=(vtind/vbind)^(1/timediffind)-1
label variable cagrind "Compound Annual Growth Rate"
generate v2030ei=vtind*(1+cagrind)^(2030-year)
label variable v2030ei "Expected Value at 2030"
generate progressi= (v2030ei-vbind)/(targetvalue-vbind)
label variable progressi "Progress"
generate progresscati=0 if progressi<0
replace progresscati=1 if progressi>=0 & progressi<0.1
replace progresscati=2 if progressi>=0.1 & progressi<0.5
replace progresscati=3 if progressi>=0.5 & progressi<0.95
replace progresscati=4 if progressi>=0.95 & progressi!=. | vbind>=targetvalue
label define progressi 0 "Regression" 1 "Stagnation" 2 "Marginal progress" 3 "Moderate progress" 4 "On track/target met"
label values progresscati progressi
sort country year
list country year value cagrind v2030ei progressi progresscati, sepby (country)
bysort year: tabulate progresscati, missing
list country year value cagrind progressi progresscati if progresscati==., sepby (country)
export excel country year uncontinentalregion income_group value cagrind progressi progresscati using "Access to Safe Water/Progress/Progress Assessment Each Year.xlsx", firstrow(varlabels) replace

log close
