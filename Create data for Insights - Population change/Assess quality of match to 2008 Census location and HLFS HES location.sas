** Cross-validate the TA assignment rules against 5-years-ago 2013 Census location (i.e. 2008), HLFS, HES;
** Also output summary match statistics for 2013 Census location;

** Start out by cleaning up the work library;
proc datasets lib=work nolist;
	save formats;
quit;
run;

** Now assign TAs to the 2008 data;
proc sql;
	create table cenurp_2008_ta as
	select a.*, b.cen_ind_sex_code as sex, input(b.cen_ind_age_code,3.)-5 as age
	from project.cenurp_mar2008_ta a left join cen.census_individual b
	on a.snz_uid = b.snz_uid;
quit;

data project.source_match_2008(keep=snz_uid sex age ta source lag lagdays lagmths source_ta match startdate source_date);
	length source $ 6;
	set cenurp_2008_ta;
	where source not in ('hes','hlfs');
	format age age_gp_broad. source_date startdate date9.;
run;

** Macro to assign a TA location to SNZ_UIDs given an input dataset containing potential locations;
%include "&path.\assign_location_macro.sas";

** Run allocation to TAs for 2008 location records from 2013 Census;
%assign_location(indat=project.source_match_2008,outdat=project.cen_idi_ta_2008);

data project.cen_idi_ta_2008(drop=cen_ind_age_code cen_ind_sex_code);
	merge project.censusaddress5(in=a drop=source) project.cen_idi_ta_2008 cen.census_individual(keep=snz_uid cen_ind_age_code cen_ind_sex_code);
	by snz_uid;
	if a;
	if ta=pred_ta then match=1;
	else if pred_ta='' then match=.;
	else match=0;
	age=input(cen_ind_age_code,3.)-5;
	sex=cen_ind_sex_code;
	** Some 5-years-ago addresses were not able to be coded below TA level (identified by an AU code beginning with 999. 
	** The quality of these addresses may be worse so we want to identify these and describe separately;
	if substr(au5,1,3)='999' then au_detail='N';
	else au_detail='Y';
run;

** Summarise the match on TA for both 2008 and 2013 years based on 2013 Census;
%macro assess_ta(year);
%let erpdat=MAR&year.;
proc tabulate data=project.cen_idi_ta_&year. missing out=census_match&year.(drop=_type_ _page_ _table_);
	class lag match source ta age sex %if &year.=2008 %then %do; au5_code au_detail %end; ;
	tables lag all,match*N / nocellmerge;
	tables source,match*N / nocellmerge;
	tables ta,match*N / nocellmerge;
	%if &year.=2008 %then %do; tables au5_code,match*N / nocellmerge;
	tables au_detail,match*N / nocellmerge; %end;
	tables sex*age,match*N / nocellmerge;
	title "IDI prioritised TA match to Census address, March &year. - By lag, source, priority, TA, age and sex";
run;

data census_match&year.;
	set census_match&year.;
	if N < 6 then N=0;
run;

%rr3(census_match&year.,census_match&year.,N);

proc export data=census_match&year. replace outfile="&path.\Tables\IDI prioritised match with 2013 Census year=&year. by key characteristics.csv" dbms=csv;
run;
%mend assess_ta;

%assess_ta(2013);
%assess_ta(2008);

** Now we want to compare how well the data matches in terms of internal migration between TAs - i.e. changes between TAs over the 5 years to March 2013;
data project.cen_idi_ta_2008_2013;
	merge project.cen_idi_ta_2008(in=a keep=snz_uid pred_ta ta source rename=(pred_ta=pred_ta5 ta=ta5 source=source5)) project.cen_idi_ta_2013(in=b keep=snz_uid pred_ta ta match source);
	by snz_uid;
	if a and b and pred_ta ne '' and pred_ta5 ne '';
	** Firstly did they have a move between TAs according to Census?;
	if ta ne '' and ta5 ne '' then do;
		if ta=ta5 then move=0;
		else move=1;
	end;
	** Now did they have a move between TAs according to IDI?;
	if pred_ta ne '' and pred_ta5 ne '' then do;
		if pred_ta=pred_ta5 then idi_move=0;
		else idi_move=1;
	end;
	** Now did the Census and IDI TAs match in both periods;
	if ta ne '' and ta5 ne '' and pred_ta ne '' and pred_ta5 ne '' then do;
		if ta=pred_ta and ta5=pred_ta5 then match_move=1;
		else match_move=0;
	end;
	format move idi_move match_move match yn. ta pred_ta ta5 pred_ta5 $ta.;
run;

** Summarise the extent TA changes match at an individual level across the 5 years;
** Could also be worth splitting this by whether or not 2008 location was recorded below TA level or not but haven't done that here;
proc tabulate data=project.cen_idi_ta_2008_2013 missing out=idi_cen_move(drop=_type_ _page_ _table_);
	class move match idi_move match_move;
	tables move,idi_move,match_move*N / nocellmerge;
	*tables move,match_move*N / nocellmerge;
	tables move,idi_move,match*N / nocellmerge;
	title "Census moves from 2008 to 2013 by whether they also have an IDI move";
run;

data idi_cen_move;
	set idi_cen_move;
	if N < 6 then N=0;
run;

%rr3(idi_cen_move,idi_cen_move,N);

proc export data=idi_cen_move replace outfile="&path.\Tables\Census IDI TA movement comparison.csv" dbms=csv;
run;

** Now compare IDI and Census internal migration figures at an aggregate level for the same population who have TAs recorded at both times in both sources;
proc tabulate data=project.cen_idi_ta_2008_2013 missing out=moves_ta_2008_2013(drop=_type_ _page_ _table_);
	class move match idi_move match_move ta ta5 pred_ta pred_ta5;
	tables ta, move*N / nocellmerge;
	tables ta5, move*N / nocellmerge;
	tables pred_ta, idi_move*N / nocellmerge;
	tables pred_ta5, idi_move*N / nocellmerge;
run;

data moves_ta_2008_2013;
	set moves_ta_2008_2013;
	if N < 6 then N=0;
run;

%rr3(moves_ta_2008_2013,moves_ta_2008_2013,N);

proc export data=moves_ta_2008_2013 replace outfile="&path.\Tables\Census and IDI internal migration numbers by TA.csv" dbms=csv;
run;

** Now assess the quality of the HLFS and HES match over time;
data project.hlfs_hes(rename=(source=survey_source));
	set project.HLFSAddress project.HESAddress;
	where startdate <= '01jul2016'd;
run;

proc sort data=project.hlfs_hes;
	by snz_uid startdate;
run;

data project.hlfs_hes;
	set project.hlfs_hes;
	by snz_uid;
	if last.snz_uid;
run;

data project.hlfs_hes_ta;
snz_uid=.;
run;

** Firstly need to match on potential sources of location for each HES or HLFS respondent for the survey period - as before with Census;
%macro addsourceta(source=);
proc sql;
	create table source_before as
	select a.snz_uid, a.ta, a.startdate, b.TA as source_TA , b.startdate as source_date, "&source." as source
	%if &source.= moeter %then %do; , b.provider %end; from
	project.hlfs_hes a inner join project.&source.address b
 	on a.snz_uid=b.snz_uid and b.startdate < a.startdate and b.ta ne '999'
	order by snz_uid, source_date descending;
quit;

data source_before;
	set source_before;
	by snz_uid;
	if first.snz_uid;
run;

proc sql;
	create table source_after as
	select a.snz_uid, a.ta, a.startdate, b.TA as source_TA, b.startdate as source_date, "&source." as source
	%if &source.= moeter %then %do; , b.provider %end; from
	project.hlfs_hes a inner join project.&source.address b
 	on a.snz_uid=b.snz_uid and b.startdate >= a.startdate and b.ta ne '999'
	order by snz_uid, source_date;
quit;

data source_after;
	set source_after;
	by snz_uid;
	if first.snz_uid;
run;

data source;
	length source $ 6;
	set source_before source_after;
	by snz_uid;
run;

data project.hlfs_hes_ta;
	set project.hlfs_hes_ta source;
	by snz_uid;
	if snz_uid ne .;
run;
%mend addsourceta;

%addsourceta(source=acc);
%addsourceta(source=dlr);
*%addsourceta(source=hnzn);
*%addsourceta(source=hnzr);
*%addsourceta(source=hnzt);
%addsourceta(source=ird);
%addsourceta(source=moe);
%addsourceta(source=moesch);
%addsourceta(source=moeter);
%addsourceta(source=msdr);
*%addsourceta(source=msdp);
%addsourceta(source=mvr);
%addsourceta(source=nhi);
%addsourceta(source=pho);
%addsourceta(source=plta);
%addsourceta(source=pltd);
%addsourceta(source=slair);
*%addsourceta(source=slap);
*%addsourceta(source=slas);

** Now need to do employment TA;
** Firstly select employment records that overlap the date of interest;
proc sql;
	create table source_overlap as
	select a.snz_uid, a.ta, a.startdate, b.TA as source_TA, a.startdate as source_date, "emp" as source from
	project.hlfs_hes a inner join project.empaddress b
 	on a.snz_uid=b.snz_uid and b.firstdate <= a.startdate <= b.lastdate
	order by snz_uid, source_date descending;
quit;

data source_overlap;
	set source_overlap;
	by snz_uid;
	if first.snz_uid;
run;

proc sql;
	create table source_before as
	select a.snz_uid, a.ta, a.startdate, b.TA as source_TA, b.firstdate, b.lastdate, "emp" as source from
	project.hlfs_hes a inner join project.empaddress b
 	on a.snz_uid=b.snz_uid and b.lastdate < a.startdate 
	order by snz_uid, lastdate descending;
quit;

data source_before(drop=firstdate lastdate);
	set source_before;
	by snz_uid;
	if first.snz_uid;
	source_date=lastdate;
	format source_date date9.;
run;

proc sql;
	create table source_after as
	select a.snz_uid, a.ta, a.startdate, b.TA as source_TA, b.firstdate, b.lastdate, "emp" as source from
	project.hlfs_hes a inner join project.empaddress b
 	on a.snz_uid=b.snz_uid and b.firstdate > a.startdate 
	order by snz_uid, firstdate;
quit;

data source_after(drop=firstdate lastdate);
	set source_after;
	by snz_uid;
	if first.snz_uid;
	source_date=firstdate;
	format source_date date9.;
run;

data source2;
	length source $ 6;
	set source_before source_after;
	by snz_uid;
run;

** Only use employment before and after if not in the overlap group;
data source2;
	merge source_overlap(in=a keep=snz_uid) source2;
	by snz_uid;
	if not a;
run;

data project.hlfs_hes_ta;
	set project.hlfs_hes_ta source_overlap source2;
	by snz_uid;
	if snz_uid ne .;
run;

** Now we want to look at the time between the address update and the HLFS HES date;
data project.hlfs_hes_ta;
	set project.hlfs_hes_ta;
	** Fix Auckland and Chch TAs in all sources;
	if ta in ('004','005','006','007','008','009','010') then ta='076';
	else if ta = '061' then ta='060';
	if source_ta in ('004','005','006','007','008','009','010') then source_ta='076';
	else if source_ta = '061' then source_ta='060';
	lag=floor((startdate-source_date)/365.25);
	lagdays=startdate-source_date;
	lagmths=floor((startdate-source_date)/365.25*12);
	if source_ta=ta then match=1;
	else if source_ta=. then match=.;
	else match=0;
	if lag>10 or lag<-5 then delete;
run;

proc sql;
	create table project.hlfs_hes_ta as
	select a.*, b.provider as provider_keep
	from project.hlfs_hes_ta a left join project.providers_keep b
	on a.provider = b.provider
	order by snz_uid;
quit;

data project.hlfs_hes_ta;
	set project.hlfs_hes_ta;
	if source='moeter' and provider ne '' and provider_keep='' then delete;
run;

proc sql;
	create table project.hlfs_hes_ta as
	select a.*, b.ta as emp_ta
	from project.hlfs_hes_ta a left join project.employers_keep b
	on a.ta = b.ta and a.source = 'emp'
	order by snz_uid;
quit;

data project.hlfs_hes_ta(drop=emp_ta);
	set project.hlfs_hes_ta;
	if emp_ta='' and source='emp' then delete;
run;

** Now assign TAs to the HLFS HES data;
proc sql;
	create table hlfs_hes_ta as
	select a.*, b.hlfs_urd_sex_code as sex, b.hlfs_urd_age_nbr as age
	from project.hlfs_hes_ta a left join hlfs.data b
	on a.snz_uid = b.snz_uid and intnx('month', input(b.hlfs_urd_quarter_date,yymmdd10.), -2) = a.startdate;
quit;

proc sql;
	create table hlfs_hes_ta as
	select a.*, b.hes_per_sex_snz_code as hes_sex, b.hes_per_age_nbr as hes_age
	from hlfs_hes_ta a left join hes.hes_person b
	on a.snz_uid = b.snz_uid
	order by snz_uid;
quit;

** Where someone has responded in multiple periods only take the first
** People can stay in the HLFS panel from one period to the next but we don't want to assess them multiple times;
data project.hlfs_hes_ta(drop=hes_age hes_sex);
	set hlfs_hes_ta;
	if age=. then age=hes_age;
	if age<0 and age ne . then age=0;
	if sex='' then sex=hes_sex;
	if age=. or sex='' then delete;
	format age age_gp_broad. source_date startdate date9.;
run;

** Now assign a probability of match to all addresses - run the location assignment algorithm;
%assign_location(indat=project.hlfs_hes_ta,outdat=project.hlfs_hes_idi);

proc sort data=project.hlfs_hes; by snz_uid StartDate; run;
proc sort data=project.hlfs_hes_idi; by snz_uid StartDate; run;

** Add age and sex from HLFS and HES data - this includes people without an IDI location derived;
proc sql;
	create table hlfs_hes as
	select a.*, b.hlfs_urd_sex_code as sex, b.hlfs_urd_age_nbr as age
	from project.hlfs_hes a left join hlfs.data b
	on a.snz_uid = b.snz_uid and intnx('month', input(b.hlfs_urd_quarter_date,yymmdd10.), -2) = a.startdate;
quit;

proc sql;
	create table hlfs_hes as
	select a.*, b.hes_per_sex_snz_code as hes_sex, b.hes_per_age_nbr as hes_age
	from hlfs_hes a left join hes.hes_person b
	on a.snz_uid = b.snz_uid
	order by snz_uid;
quit;

** Re-code age and sex;
data hlfs_hes(drop=hes_age hes_sex);
	set hlfs_hes;
	if age=. then age=hes_age;
	if age<0 and age ne . then age=0;
	if sex='' then sex=hes_sex;
	if age=. or sex='' then delete;
	format age age_gp_broad.;
run;

** Assess whether the HLFS HES TA matches the IDI-based TA allocated;
data project.hlfs_hes_idi_ta;
	merge hlfs_hes project.hlfs_hes_idi;
	by snz_uid startdate;
	if ta=pred_ta then match=1;
	else if pred_ta='' then match=.;
	else match=0;
	startyear=year(startdate);
run;

proc sort data=project.hlfs_hes_idi_ta;
	by survey_source;
run;

** Summarise match by characteristics and time;
proc tabulate data=project.hlfs_hes_idi_ta missing out=hlfshes_match(drop=_type_ _page_ _table_);
	by survey_source;
	class startdate startyear lag match source ta age sex;
	tables startyear all,match*N / nocellmerge;
	tables lag,match*N / nocellmerge;
	tables source,match*N / nocellmerge;
	tables sex,match*N / nocellmerge;
	title "IDI prioritised TA match to HLFS HES address - By survey quarter and year, lag, source, priority, TA, age and sex";
run;

data hlfshes_match;
	set hlfshes_match;
	if N < 6 then N=0;
run;

%rr3(hlfshes_match,hlfshes_match,N);

proc export data=hlfshes_match replace outfile="&path.\Tables\IDI prioritised match with HLFS and HES by key characteristics.csv" dbms=csv;
run;