*Edited June 22nd to add % native americans
*Edited June 2nd to fix missing state data in Nationalraw file;
*Edited June 12 to assign counties with no COVID data as -1 ;


*Month and day of current downloand;
%let month=12;
%let day=31;
%let year=2022;
*Data directory - shivani; 

%let onedrive=h:\dashboard\Data;
%let datadir=O:\dashboard\data\download;  *datadir is your preferred local path;
*================= CODE TO IMPORTING Datasets FOR COUNTY DASHBOARD ==================;

* Data set list
*NYtimes
*NCHS - CDC wonder
1. ACS 2018 5yr
2. ACS 2018 1yr
3. CDC Diabetes Surveillance
4. CDC SVI
5. SAHIE
;

*Daesung's Emory Virtual desktop;
*%let datadir = H:\COVID-Dashboard\Data;


*%let datadir = H:\COVID-Dashboard;


*================= COVID DEATHS =======================================;
*data inputs: NYTimes COVID deaths by states and county - as of May 2;
*data can be found at:
	*https://github.com/nytimes/covid-19-data;

		filename usny temp;
		proc http
		 url="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"
		 method="GET"
		 out=usny;
		run;
		proc import
		  file=usny
		  out=work.USnytimes replace
		  dbms=csv;
		run;


		filename statesny temp;
		proc http
		 url="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
		 method="GET"
		 out=statesny;
		run;
		proc import
		  file=statesny
		  out=work.statesnytimes replace
		  dbms=csv;
		run;

*
		filename countyny temp;
		*proc http
		 url="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
		 method="GET"
		 out=countyny;
		*run;
		*proc import
		  file=countyny
		  out=work.countiesnytimes replace
		  dbms=csv;
		*run;


      
    

filename first temp;
		proc http
		 url="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2020.csv"
		 method="GET"
		 out=first;
		run;
		proc import
		  file=first
		  out=work.countiesnytimes2020 replace
		  dbms=csv;
		run;



		filename second temp;
		proc http
		 url="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2021.csv"
		 method="GET"
		 out=second;
		run;
		proc import
		  file=second
		  out=work.countiesnytimes2021 replace
		  dbms=csv;
		run;



		filename third temp;
		proc http
		 url="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2022.csv"
		 method="GET"
		 out=third;
		run;
		proc import
		  file=third
		  out=work.countiesnytimes2022 replace
		  dbms=csv;
		run;

    data countiesnytimes;
	set countiesnytimes2020 countiesnytimes2021 countiesnytimes2022;
	run;



			data covid_all0;
			length state $35.;
			set Countiesnytimes (in=c) statesnytimes (in=s) USnytimes (in=n);
			where year(date)=&year. and month(date)=&month. and day(date)=&day.;
			fips_code=put(fips,z5.);
			if c=1 then countyx=(substr(fips_code,3,3))+0;
			if c=1 then statex=(substr(fips_code,1,2))+0;
			if s=1 then statex=fips;
			if n=1 then nation=1; *indicator for nation;
			if statex in (66 69 72 78) then delete;
			drop state county fips_code;
			run;

		*Cumulative deaths due to covid by county, state, and nation;
			data covid_all;
			set covid_all0;
			state=statex;
			county=countyx;
			drop statex countyx;
			run;

			proc print data=covid_all;
			where county=.;
			var date state cases deaths;
			run;
*
PROC EXPORT DATA= WORK.covid_all ;
           * OUTFILE= "&datadir.\covid_all.csv" ;
           * DBMS=CSV REPLACE;
    * PUTNAMES=YES;
*RUN; 
*
proc import datafile = "&onedrive.\Processed\iowa_oklahoma.csv" out = iowa
DBMS =  csv replace;
*run;
*proc sort data=iowa;*by county;*run;
*proc sort data=Countiesnytimes;*by county;*run;

*data Countiesnytimes;
* countiesnytimes iowa;
*run;

*PROC PRINT data=iowa;
*run;


*=========================== daily cases data;
		proc sort data=Countiesnytimes; by fips date; run;
		data county_daily; 
		set Countiesnytimes;
		by fips date;
		yesterdaydeaths=lag(deaths);
		yesterdaycases=lag(cases);

		if first.county then yesterdaydeaths=0;
		dailydeaths=deaths-yesterdaydeaths;
		*Setting negatives to missing;
		if dailydeaths<0 then dailydeaths=0;

		if first.county then mean7daydeaths=0;
		mean7daydeaths = mean(dailydeaths, lag (dailydeaths), lag2(dailydeaths), lag3(dailydeaths),
		                   lag4(dailydeaths), lag5(dailydeaths), lag6(dailydeaths));

		if first.county then yesterdaycases=0;
		dailycases=cases-yesterdaycases;
		*Setting negatives to missing;
		if dailycases<0 then dailycases=0;

		if first.county then mean7daycases=0;
		mean7daycases = mean(dailycases, lag (dailycases), lag2(dailycases), lag3(dailycases),
		                   lag4(dailycases), lag5(dailycases), lag6(dailycases));



		*14 day change - cases;
		percent14dayCases=100*(cases-lag14(cases))/lag14(cases);
		percent14dayDailyCases=100*(mean7daycases-lag14(mean7daycases))/lag14(mean7daycases);

		*14 day change - deaths;
		percent14dayDeaths=100*(deaths-lag14(deaths))/lag14(deaths);
		percent14dayDailyDeaths=100*(mean7daydeaths-lag14(mean7daydeaths))/lag14(mean7daydeaths);

		zero=0;


		countycode =put(fips, z5.);
		countyx=(substr(countycode,3,3))+0;
		statex=(substr(countycode,1,2))+0;
		if countyx=. and statex=. then delete; 
		drop state county;
		run;

		proc sort data=statesnytimes; by fips date; run;
		data state_daily; 
		set statesnytimes;
		by fips date;
		yesterdaydeaths=lag(deaths);
		yesterdaycases=lag(cases);

		if first.state then yesterdaydeaths=0;
		dailydeaths=deaths-yesterdaydeaths;
		*Setting negatives to missing;
		if dailydeaths<0 then dailydeaths=0;

		if first.state then mean7daydeaths=.;
		mean7daydeaths = mean(dailydeaths, lag (dailydeaths), lag2(dailydeaths), lag3(dailydeaths),
		                   lag4(dailydeaths), lag5(dailydeaths), lag6(dailydeaths));

		if first.state then yesterdaycases=0;
		dailycases=cases-yesterdaycases;
		*Setting negatives to missing;
		if dailycases<0 then dailycases=0;

		if first.state then mean7daycases=0;
		mean7daycases = mean(dailycases, lag (dailycases), lag2(dailycases), lag3(dailycases),
		                   lag4(dailycases), lag5(dailycases), lag6(dailycases));

		zero=0;
		
		*14 day change - cases;
		percent14dayCases=100*(cases-lag14(cases))/lag14(cases);
		percent14dayDailyCases=100*(mean7daycases-lag14(mean7daycases))/lag14(mean7daycases);

		*14 day change - deaths;
		percent14dayDeaths=100*(deaths-lag14(deaths))/lag14(deaths);
		percent14dayDailyDeaths=100*(mean7daydeaths-lag14(mean7daydeaths))/lag14(mean7daydeaths);

		statex=fips;
		if statex in (66 69 72 78) then delete;
		drop state ;
		run;

	proc sort data=usnytimes; by date; run;
		data us_daily; 
		set usnytimes;
		by date;
		yesterdaydeaths=lag(deaths);
		yesterdaycases=lag(cases);

		if _n_=1 then yesterdaydeaths=0;
		dailydeaths=deaths-yesterdaydeaths;
		*Setting negatives to missing;
		if dailydeaths<0 then dailydeaths=0;


		if _n_=1 then mean7daydeaths=0;
		mean7daydeaths = mean(dailydeaths, lag (dailydeaths), lag2(dailydeaths), lag3(dailydeaths),
		                   lag4(dailydeaths), lag5(dailydeaths), lag6(dailydeaths));


		if _n_=1 then yesterdaycases=0;
		dailycases=cases-yesterdaycases;
		*Setting negatives to missing;
		if dailycases<0 then dailycases=0;

		if _n_=1 then mean7day=0;
		mean7daycases = mean(dailycases, lag (dailycases), lag2(dailycases), lag3(dailycases),
		                   lag4(dailycases), lag5(dailycases), lag6(dailycases));

		*14 day change - cases;
		percent14dayCases=100*(cases-lag14(cases))/lag14(cases);
		percent14dayDailyCases=100*(mean7daycases-lag14(mean7daycases))/lag14(mean7daycases);

		*14 day change - deaths;
		percent14dayDeaths=100*(deaths-lag14(deaths))/lag14(deaths);
		percent14dayDailyDeaths=100*(mean7daydeaths-lag14(mean7daydeaths))/lag14(mean7daydeaths);


		zero=0;
        nation=1;
		run;

		data mean7day_current;
		set state_daily county_daily us_daily;
		where  year(date)=&year. and month(date)=&month. and day(date)=&day.;

  		state=statex;
		county=countyx;
		if state in (66 69 72 78) then delete;
		keep state county nation date dailydeaths mean7daydeaths percent14dayCases percent14dayDailyCases percent14dayDeaths percent14dayDailyDeaths dailycases mean7daycases zero;
		run;

		data covidtimeseries;
		set state_daily county_daily us_daily;
		where month(date)>3 or year(date)=2023 or year(date)=2022 or year(date)=2021;
  		state=statex;
		county=countyx;
		if state in (66 69 72 78) then delete;
		*keep state county nation date dailydeaths mean7daydeaths dailycases mean7daycases zero;
		drop statex countyx;
		run;
		proc sort data=covidtimeseries; by nation state county date; run;

			proc print data=mean7day_current;
			where county=.;
			var date state dailydeaths mean7daydeaths dailycases;
			run;


*****************************************************************
*Population denominator and 2013 Urban rural codes - From CDC Wonder
*https://www.cdc.gov/nchs/data_access/urban_rural.htm#update;
*****************************************************************;
      PROC IMPORT OUT= WORK.urbancodes                         
                        DATAFILE= "&onedrive.\Raw\CDC_Urban_Rural\Urban_Rural_2013_Classification.xlsx" 
                        DBMS=excel REPLACE;
                 RANGE="SAS$"; 
                 GETNAMES=YES;
                 MIXED=No;
                 SCANTEXT=YES;
                 USEDATE=YES;
                 SCANTIME=YES;
            RUN;

			/*
proc import datafile = "&onedrive.\Raw\CDC_Urban_Rural\Urban_Rural_2013_Classification.csv" out = urbancodes
DBMS =  csv replace;
guessingrows=32767;
run;
			*/




			data urbancodes2;
			set urbancodes;
				fips=put(county_code,z5.);
				countyx=(substr(fips,3,3))+0;
				statex=(substr(fips,1,2))+0;
				countyname=county;
			drop county fips;
			run;

			data urbancodes3;
			set urbancodes2;
			county=countyx;
			state=statex;

			countynum=county_code+0;
			countycode=put(countynum, z5.);
			statecode=substr(countycode,1,2);

			annualdeaths2018=deaths;
			annualmortality2018= crude_rate;
			urbanrural=compress(_013_Urbanization_Code||_013_Urbanization_Code);*changed this line to make the code work;
			if _2013_Urbanization_Code = . then urbanrural= " ";
			drop statex countyx;
			run;


 				PROC IMPORT OUT= WORK.statepopulation                         
                        DATAFILE= "&onedrive.\Raw\CDC_Urban_Rural\State_2018_pop.xlsx" 
                        DBMS=EXCEL REPLACE;
                 RANGE="SAS$"; 
                 GETNAMES=YES;
                 MIXED=No;
                 SCANTEXT=YES;
                 USEDATE=YES;
                 SCANTIME=YES;
            RUN;

/*proc import datafile = "O:\COVID19_data_shared\CDC_Urban_Rural\State_2018_pop.csv" out = statepopulation
DBMS =  csv replace;
run;*/



			data statepopulation2;
			set statepopulation;
				statex=state_code;
				statename=state;
			drop state;
			run;

			data statepopulation3;
			set statepopulation2;
				state=state_code;
			drop statex state_code;
			run;

		proc means data=statepopulation3 sum ;
		ods output summary=nationalpop;
		run;

		data nationalpop2;
		set nationalpop;
		population=Population_Sum;
		state = .;
		county =.;
		nation=1;
		keep  population state nation county;
		run;

/*
proc compare data=urbancodes4(obs=0) compare=statepopulation3(obs=0) compare=nationalpop2(obs=0);
run;
data urbancodes4;
set urbancodes3;
Deaths1=input(Deaths,best32.);
Population1=input(Population,best32.);
Crude_Rate1=input(Crude_Rate,best32.);
drop Deaths Population Crude_Rate;
rename Deaths1=Deaths Population1=Population Crude_Rate1=Crude_Rate;
run;
		*/




			data population_all;
			set urbancodes3 statepopulation3 nationalpop2;
			deaths_allcause=deaths;
			drop deaths;
			run;


*****************************************************************************
			COVID outcome data with population denominator 
*****************************************************************************;

			
	proc sort data=population_all; by nation state county ; run;
	proc sort data=covid_all; by nation state county ; run;
	proc sort data=mean7day_current; by nation state county ; run;
	

	data covid_pop;
	merge population_all covid_all (in=covid) mean7day_current ;
	by nation state county;

	covidmortality = 100000*deaths/population;
	caserate = 100000*cases/population;

	covidmortality7day= 100000*mean7daydeaths/population;
	caserate7day= 100000*mean7daycases/population;


	*Revising all the 0's to be -1 for the figures;
	array acovid 		cases 		deaths 		covidmortality 		caserate 	covidmortality7day 		caserate7day;
	array acovidfigs 	casesfig 	deathsfig 	covidmortalityfig 	caseratefig covidmortality7dayfig 	caserate7dayfig;

	do over acovid;
	acovidfigs=acovid;
	if acovid in (0 .) then acovidfigs=-1;
	end;

	refzero=0;
    if state=0 or (nation=. and state=. and county=.) then delete;
	drop fips;
	run;



			proc print data=covid_pop;
			where county=.;
			var date state county nation cases deaths;
			run;



	*data checking;
			proc means data =covid_pop n nmiss min max mean;
			var  deaths   covidmortality7day   deathsfig covidmortalityfig  covidmortality7dayfig ;
			run;

			proc means data =covid_pop n nmiss min max mean;
			where deaths in (0 .);
			var  deaths     deathsfig covidmortalityfig   ;
			run;

			proc means data =covid_pop n nmiss min max mean;
			where deaths notin (0 .);
			var  deaths     deathsfig  covidmortalityfig ;
			run;

			proc means data =covid_pop n nmiss min max mean;
			where dailydeaths in (0 .) ;
			var    covidmortality7day  covidmortality7dayfig ;
			run;

			proc means data =covid_pop n nmiss min max mean;
			where dailydeaths notin (0 .);
			var    covidmortality7day    covidmortality7dayfig ;
			run;


			proc means data =covid_pop n nmiss min max mean;
			where cases in (0 .);
			var cases  caserate  casesfig  caseratefig  ;
			run;

			proc means data =covid_pop n nmiss min max mean;
			where cases notin (0 .) and caserate ne .;
			var cases  caserate  casesfig  caseratefig  ;
			run;

			*proc means data =covid_popn nmiss min max mean;
			*where dailycases in (0 .) ;
			*var caserate7day caserate7dayfig;
			*run;


			proc print data =covid_pop;
			where casesfig <=1 ;
			var statename covidmortality countyname population cases deaths covidmortality caserate covidmortality7day caserate7day casesfig deathsfig covidmortalityfig caseratefig covidmortality7dayfig caserate7dayfig deaths nation ;
			run;

			*Looking at the distribtuion of the quantiles for plotting;
			proc rank data=covid_pop
			groups=7 out=ranks;
			where covidmortalityfig ne -1;
			var covidmortalityfig;
			ranks covidmortalityfigHept;
			run;

			proc means data=ranks;
			class covidmortalityfigHept;
			var covidmortality;
			run;


	proc sort data=population_all; by nation state county ; run;
	proc sort data=covidtimeseries; by nation state county ; run;
	data covidtimeseries_pop;
	merge population_all covidtimeseries (in=covid);
	by nation state county;

	covidmortality = 100000*deaths/population;
	caserate = 100000*cases/population;

	dailycaserate=100000*dailycases/population;
    dailymortality=100000*dailydeaths/population;


	covidmortality7day= 100000*mean7daydeaths/population;
	caserate7day= 100000*mean7daycases/population;

	
	*case fatality ratio;
	cfr=100*deaths/cases;

	*Revising all the 0's to be -1 for the figures;
	array acovid 		cases 		deaths 		covidmortality 		dailycaserate  		dailymortality 		caserate 	covidmortality7day 		caserate7day;
	array acovidfigs 	casesfig 	deathsfig 	covidmortalityfig 	dailycaseratefig  	dailymortalityfig 	caseratefig covidmortality7dayfig 	caserate7dayfig;

	do over acovid;
	acovidfigs=acovid;
	if acovid in (0 .) then acovidfigs=-1;
	end;

	refzero=0;
	level=compress(state||county||nation);
    if state=0 or (nation=. and state=. and county=.) then delete;
	run;

data Covidtimeseries_pop1;
set Covidtimeseries_pop;
length _2013_Urbanization $100.;
run;
proc freq data=Covidtimeseries_pop1;
tables _2013_Urbanization;
run;





PROC EXPORT DATA= WORK.Covidtimeseries_pop 
            OUTFILE= "\\apporto.com\dfs\RSPHEmory\Users\zzha736_rsphemory\Desktop\covidtimeseries00.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;



*************************************************************;
* Date: May 2020
* Data: American Community Survey 2014-2018 5-Year Estimates, Nation, State and County
* Data Source: Social explorer 
*************************************************************;
	Data acs2018_5yr_county; 
	set "&onedrive.\raw\ACS 2018 5yr\acs2018_5yr_county"; 

	popden = A00002_002;
	age65over = (A01001_011 + A01001_012 + A01001_013) / A00001_001 * 100;
	male      = A02001_002 / A00001_001 * 100;
	female    = A02001_003 / A00001_001 * 100;
	black     = A04001_004 / A00001_001 * 100;  
	hispanic  = A04001_010 / A00001_001 * 100;
	minority = (A00001_001-A04001_003)/ A00001_001 * 100;
    natives =   A03001_004/ A00001_001 * 100;
	groupquater = A19001_002/A00001_001 * 100;
	college =     A12001_005/A12001_001 * 100;
	hhincome = A14008_001 ;
	poverty = A13005_002 / A13005_001 * 100; 

	county=(substr(fips,3,3))+0;
	state=(substr(fips,1,2))+0;
	keep county state popden age65over  male  female black hispanic minority groupquater college hhincome poverty natives;
	run;

	Data acs2018_5yr_state; 
	set "&onedrive.\raw\ACS 2018 5yr\acs2018_5yr_state"; 

	popden = A00002_002;
	age65over = (A01001_011 + A01001_012 + A01001_013) / A00001_001 * 100;
	male      = A02001_002 / A00001_001 * 100;
	female    = A02001_003 / A00001_001 * 100;
	black     = A04001_004 / A00001_001 * 100;  
	hispanic  = A04001_010 / A00001_001 * 100;
	minority = (A00001_001-A04001_003)/ A00001_001 * 100; 
	natives =   A03001_004/ A00001_001 * 100; 
	groupquater = A19001_002/A00001_001 * 100;
	college =     A12001_005/A12001_001 * 100;
	hhincome = A14008_001 ;
	poverty = A13005_002 / A13005_001 * 100; 
	
	county = " ";
	state = (substr(fips,1,2))+0;
	keep state county popden age65over  male  female black hispanic minority groupquater college hhincome poverty natives;
	run;

	Data acs2018_5yr_nation; 
	set "&onedrive.\raw\ACS 2018 5yr\acs2018_5yr_nation"; 

	popden = A00002_002;
	age65over = (A01001_011 + A01001_012 + A01001_013) / A00001_001 * 100;
	male      = A02001_002 / A00001_001 * 100;
	female    = A02001_003 / A00001_001 * 100;
	black     = A04001_004 / A00001_001 * 100;  
	hispanic  = A04001_010 / A00001_001 * 100;
	minority = (A00001_001-A04001_003)/ A00001_001 * 100; 
	natives =   A03001_004/ A00001_001 * 100;  
	groupquater = A19001_002/A00001_001 * 100;
	college =     A12001_005/A12001_001 * 100;
	hhincome = A14008_001 ;
	poverty = A13005_002 / A13005_001 * 100; 

	county = " ";
	state = " ";
	nation=1;
	keep state nation county popden age65over  male  female black hispanic minority groupquater college hhincome poverty natives;
	run;

	data acs2018_5yr_all1;
	set acs2018_5yr_county  acs2018_5yr_state acs2018_5yr_nation;
	statex=input(state,5.0);
	countyx=input(county,5.0);
	drop state county;
	run;

	data acs2018_5yr_all;
	set acs2018_5yr_all1;
	state=statex;
	county=countyx;
	drop statex countyx;
	if state in (66 72 78) then delete;
	run;



*****************************************************************
CDC SVI 2018 Documentation - 1/31/2020
*****************************************************************;
	DATA svi2018_us_county;	
	SET "&onedrive.\raw\CDC SVI\svi2018_us_county";
	fipscode=put(FIPS,z5.);

	*countyname=county;
	*statename=state;
	countyx=(substr(fipscode,3,3))+0;
	statex=ST;
	if st in (66 72 78) then delete;
	keep RPL_THEME1 RPL_THEME2 RPL_THEME3 RPL_THEME4 RPL_THEMES countyx statex ;
	run;

	data SVI;
	set svi2018_us_county;
	county=countyx;
	state=statex;
	nation=.;
	drop countyx statex;
	run;



***************************************************************;
*CDC diabetes Atlas data;
***************************************************************;
		
*CDC diabetes Surveilance data;
	DATA diabetescounty1;	
	SET "&onedrive.\raw\CDC Diabetes Surveillance\county";
	fipscode=put(CountyFIPS,z5.);
	countyx=(substr(fipscode,3,3))+0;
	statex=fips;
	drop county state;
	run;
	DATA diabetescounty;	
	SET diabetescounty1;
	state=statex ;
	county=countyx;
	drop statex countyx CountyFIPS fips fipscode;
	run;

	proc means data=diabetescounty;
	where state notin (66 72 78);
	class state;
	var diabetes obesity;
	ods output summary=diabetesstate0;
	run;

	proc means data=diabetescounty;
	where state notin (66 72 78);
	var diabetes obesity;
	ods output summary=diabetesnation0;
	run;

	data diabetesnation;
	set diabetesnation0;
	diabetes=diabetes_Mean;
	obesity=obesity_mean;
	county=.;
	state=.;
	nation=1;
	keep diabetes obesity nation county state;
	run;

	data diabetesstate;
	set diabetesstate0;
	diabetes=diabetes_Mean;
	obesity=obesity_mean;
	county=.;
	nation=.;
	keep diabetes obesity nation county state;
	run;


/* these numbers don't align;
	DATA diabetesstate1;	
	SET "&datadir.\CDC Diabetes Surveillance\state"; *Need to remove the territories;
	county = .;
	statex=put(fips, 2.0);
	drop state;
	run;
	DATA diabetesnation;	
	SET "&datadir.\CDC Diabetes Surveillance\nation";
	county =.;
	state = .;
	nation=1;
	run;
*/
	
	data CDCdiabetes;
	set diabetescounty  diabetesstate diabetesnation;
	if state in (66 72 78) then delete;
	drop fips;
	run;					

*******************************************************************;
*SAIHE Health insurance files;
*******************************************************************;
	data sahie_20181;
	set "&onedrive.\raw\SAHIE\sahie_2018_new";
	statex=put(statefips,8.);
	countyx=put(countyfips,8.);
	keep statex countyx PCTUI PCTIC;
	run;

	data sahie_2018;
	set sahie_20181;
	state=input(statex,8.0);
	county=input(countyx,8.0);
	if county=0 then county=.;
	nation=.;
	drop statex countyx;
	run;

/*This file is not needed - state is covered in county;
proc means data=sahie_2018 stackods;
class state;
var PCTUI;
ods output summary=sahie_2018_state;
run;

data sahie_2018_state2;
set sahie_2018_state;
county=.;
nation=.;
PCTUI=mean;
keep state county nation PCTUI;
run;
*/


proc means data=sahie_2018 stackods;
var PCTUI;
ods output summary=sahie_2018_nat;
run;

data sahie_2018_nat2;
set sahie_2018_nat;
county=.;
state=.;
nation=1;
PCTUI=mean;
keep state county nation PCTUI;
run;

data sahie_2018_merge;
set sahie_2018 sahie_2018_nat2;
run;


*Merging the data sources;
		proc sort data=acs2018_5yr_all; by nation state county ; run;
		proc sort data=svi; by nation state county ;  run;
		proc sort data=CDCdiabetes; by nation state county ; run;
		proc sort data=sahie_2018_merge;  by nation state county ; run; 
		proc sort data=Covid_pop;  by nation state county ; run; 

		data mergedsocial;
		merge acs2018_5yr_all svi CDCdiabetes sahie_2018_merge Covid_pop;
		by nation state county ;
		cfr=100*deaths/cases;
		run;

	*data checking;
			proc print data=mergedsocial;
			where county=.;
			var date state cases deaths;
			run;

			proc means data =mergedsocial;
			where cases>0;
			var cases deaths covidmortality caserate covidmortality7day caserate7day cfr;
			run;

			proc print data =mergedsocial;
			where deaths=-1;
			var cases deaths covidmortality caserate covidmortality7day caserate7day;
			run;


proc sort data=mergedsocial; by nation state county; run;
		PROC EXPORT DATA= WORK.mergedsocial 
		            OUTFILE= "\\apporto.com\dfs\RSPHEmory\Users\zzha736_rsphemory\Desktop\nationalraw0.csv" 
		            DBMS=CSV REPLACE;
		     PUTNAMES=YES;
		RUN;


