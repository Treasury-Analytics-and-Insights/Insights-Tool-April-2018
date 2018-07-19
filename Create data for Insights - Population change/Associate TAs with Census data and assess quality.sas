** Create rules to associate TA to a person at a particular date - use 2013 Census to derive the rules and assess the quality of the match;
** 6 potential sources are excluded and the relevant code commented out as in early runs the quality is shown to be too poor and few records are useful
** These are the three HNZ sources, MSDP, SLAP, and SLAS;

** Start out by cleaning up the work library;
proc datasets lib=work nolist;
	save formats;
quit;
run;

** Firstly choose the potential TAs from each source that could be allocated to each Census record;
** For each Census record choose the most recent TA record from each source before that date, and the earliest TA record from each source after that date;
%macro census_ta(year);
%let month=03;
%let erpdat=MAR&year.;

%if &year.=2013 %then %do; %let cendata=project.CensusAddress; %end;
%else %if &year.=2008 %then %do; %let cendata=project.CensusAddress5; %end;

data project.cenurp_&erpdat._ta;
snz_uid=.;
run;

** Bring all residential addresses together for the period of interest - take the latest date before the date of interest, and the first date after it for each source;
%macro addsourceta(source=);
proc sql;
	create table source_before as
	select a.snz_uid, a.ta, a.startdate, b.TA as source_TA , b.startdate as source_date, "&source." as source
	%if &source.= moeter %then %do; , b.provider %end; from
	&cendata. a inner join project.&source.address b
 	on a.snz_uid=b.snz_uid and b.startdate < mdy(%eval(&month.+1),01,&year.) and b.ta ne '999'
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
	&cendata. a inner join project.&source.address b
 	on a.snz_uid=b.snz_uid and b.startdate >= mdy(%eval(&month.+1),01,&year.) and b.ta ne '999'
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

data project.cenurp_&erpdat._ta;
	set project.cenurp_&erpdat._ta source;
	by snz_uid;
	if snz_uid ne .;
run;
%mend addsourceta;

%addsourceta(source=acc);
%addsourceta(source=dlr);
%addsourceta(source=hes);
%addsourceta(source=hlfs);
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
	&cendata. a inner join project.empaddress b
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
	&cendata. a inner join project.empaddress b
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
	&cendata. a inner join project.empaddress b
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

data project.cenurp_&erpdat._ta;
	set project.cenurp_&erpdat._ta source_overlap source2;
	by snz_uid;
	if snz_uid ne .;
run;

** Now we want to look at the number of years between the address update and the Census date;
data project.cenurp_&erpdat._ta;
	set project.cenurp_&erpdat._ta;
	if ta in ('004','005','006','007','008','009','010') then ta='076';
	else if ta = '061' then ta='060';
	if source_ta in ('004','005','006','007','008','009','010') then source_ta='076';
	else if source_ta = '061' then source_ta='060';
	else if source_ta > '076' then delete;
	lag=floor((startdate-source_date)/365.25);
	lagdays=startdate-source_date;
	lagmths=floor((startdate-source_date)/365.25*12);
	if source_ta=ta then match=1;
	else if source_ta=. then match=.;
	else match=0;
	if lag>10 or lag<-5 then delete;
run;

** For Tertiary enrolments we use the location of the provider to allocate to TAs;

** Now have a look at the providers where MOETER does not predict TA well and we will exclude these;
%if &year.=2013 %then %do;
proc sort data=project.cenurp_&erpdat._ta(keep=provider match lag) out=providers;
	by provider;
	where provider ne '' and lag=0;
run;

proc means data=providers noprint;
	by provider;
	var match;
	output out=providers_cnt mean=prop_match;
run;

** Just keep providers where there is at least a 60% match on TA - and where there are at least 30 students;
** Use this for 2008 also;
data project.providers_keep;
	set providers_cnt;
	where _freq_>=30 and prop_match>=0.60;
run;

** Now look at whether employer is a good source of TA information in some TAs and not others;
** In some TAs people many people live in a different TA and cross TA boundaries for work. These TAs should be excluded;
%let year=2013;
%let erpdat=MAR&year.;
proc sort data=project.cenurp_&erpdat._ta(keep=ta match source lag) out=employers(drop=lag source);
	by ta;
	where source = 'emp' and lag=0;
run;

proc means data=employers noprint;
	by ta;
	var match;
	output out=employer_cnt mean=prop_match;
	format ta $ta.;
run;

proc sort data=employer_cnt;
	by prop_match;
run;

** Just keep TAs where there is at least a 60% match on TA;
** Use this for 2008 also;
data project.employers_keep;
	set employer_cnt;
	where prop_match>=0.60;
run;

/*
** Now check if the same TAs were a poor prediction in 2008;
proc sort data=project.cenurp_mar2008_ta(keep=ta match source lag) out=employers08(drop=lag source);
	by ta;
	where source = 'emp' and lag=0;
run;

proc means data=employers08 noprint;
	by ta;
	var match;
	output out=employer_cnt08 mean=prop_match08;
	format ta $ta.;
run;

proc sort data=employer_cnt;
	by ta;
run;

data matched;
	merge employer_cnt employer_cnt08;
	by ta;
	if prop_match > 0.6 then keep=1;
	else keep=0;
run;

proc sort data=matched;
	by descending prop_match ;
run;

proc print; run;
** Only 3 TAs that were over the 60% in 2013 were under in 2008 and 2 that were under in 2013 were over in 2008; **Good enough;
*/
%end;

** Delete the tertiary providers with a poor TA match, and employers in TAs which have a poor match;
proc sql;
	create table project.cenurp_&erpdat._ta as
	select a.*, b.provider as provider_keep
	from project.cenurp_&erpdat._ta a left join project.providers_keep b
	on a.provider = b.provider
	order by snz_uid;
quit;

data project.cenurp_&erpdat._ta;
	set project.cenurp_&erpdat._ta;
	if source='moeter' and provider ne '' and provider_keep='' then delete;
run;

proc sql;
	create table project.cenurp_&erpdat._ta as
	select a.*, b.ta as emp_ta
	from project.cenurp_&erpdat._ta a left join project.employers_keep b
	on a.ta = b.ta and a.source = 'emp'
	order by snz_uid;
quit;

data project.cenurp_&erpdat._ta(drop=emp_ta);
	set project.cenurp_&erpdat._ta;
	if emp_ta='' and source='emp' then delete;
run;

** Look at the match rate by source and lag for potential TA records;
proc tabulate data=project.cenurp_mar&year._ta out=cen&year._match(drop=_type_ _page_ _table_);
	class source match lag;
	title "Match rate with Census &year. year";
	tables source*lag,match*N / nocellmerge;
run;

data cen&year._match;
	set cen&year._match;
	if N < 6 then N=0;
run;

%rr3(cen&year._match,cen&year._match,N);

proc export data=cen&year._match replace outfile="&path.\Tables\Match with 2013 Census year=&year. by source and lag.csv" dbms=csv;
run;
%mend census_ta;

** Run for both Census night usual residence location and 5 years ago location;
%census_ta(2013);
%census_ta(2008);

** Add on age and sex;
proc sql;
	create table project.cenurp_2013_ta as
	select a.*, b.cen_ind_sex_code as sex, input(b.cen_ind_age_code,3.) as age
	from project.cenurp_mar2013_ta a left join cen.census_individual b
	on a.snz_uid = b.snz_uid;
quit;

** Exclude HES and HLFS as they dont add much and provide useful cross-validation data - split the data into training and validation sets;
data project.source_match(keep=snz_uid sex age ta source lag lagdays lagmths source_ta match startdate source_date random training);
	retain random;
	length source $ 6;
	set project.cenurp_2013_ta;
	by snz_uid;
	** Randomise at the SNZ_UID level;
	if first.snz_uid then random=ranuni(123);
	** Trained on half, validated on other half;
	if random < 0.5 then training=0;
	else training=1;
	where source not in ('hes','hlfs');
run;

proc freq data=project.source_match;
	tables training;
run;

** The bootstrapping takes a lot of data so delete temporary datasets;
proc datasets lib=work nolist;
	save formats;
quit;
run;

*Select all snz_uids;
proc sql;
create table snz_uids as select unique snz_uid
from project.source_match;
quit;

data _null_;
	set snz_uids;
	call symput('sampsize',_n_);
run;

proc sort data=project.source_match;
	by snz_uid source source_date;
run;

%macro bootstrap(reps);
%do i=1 %to &reps.;
** Select a bootstrap sample;
proc surveyselect data=snz_uids noprint
	method=urs reps=1 outhits sampsize=&sampsize. out=snz_uids&i.(keep=snz_uid);
run;

data source_match&i.;
	merge snz_uids&i.(in=a) project.source_match;
	by snz_uid;
	if a;
run;

** Create decision rules from a tree based on the bootstrap training sample;
proc hpsplit data=source_match&i.;
	class match source age sex ;
	model match = source lagdays age sex;
	where training=1;
	code file="&path.\Decision Rules\code&i..sas";
	rules file="&path.\Decision Rules\rules&i..txt";
run;

proc datasets lib=work;
	delete snz_uids&i. source_match&i.;
quit;
run;
%end;
%mend bootstrap;

%bootstrap(100);

%macro assign(reps);

** Now assign a probability of match to all potential TAs by assigning probabilities to all records using all bootstrap replications and averaging across them;
data project.source_match_bootstrap;
	set project.source_match;
	%do i=1 %to &reps.;
		%include "&path.\Decision Rules\code&i..sas";
		p_match&i.=p_match;
		%if &i.=&reps. %then %do;
			ave_p_match=mean(of p_match1-p_match&reps.);
			if lagdays>=0 then pos_lag=lagdays;
			else pos_lag=9999;
			if lagdays<0 then neg_lag=abs(lagdays);
			else neg_lag=9999;
			drop p_match:;
		%end;
	%end;
run;
%mend bootstrap;

%assign(100);

proc datasets lib=work nolist;
	save formats;
quit;
run;

proc sort data=project.source_match_bootstrap;
	by snz_uid ave_p_match;
run;

** Now just keep the potential TA record with the highest probability of matching the Census for each snz_uid;
data cen_idi_ta_2013(keep=snz_uid sex age ta source source_ta lag lagdays lagdays match startdate source_date ave_p_match training _node_);
	set project.source_match_bootstrap ;
	by snz_uid;
	if last.snz_uid; * Only keep the best match;
run;

** And match it to the total population - including those to whom we are unable to assign an address;
data cen_idi_ta_2013;
	length source $ 6;
	merge project.censusaddress(drop=source) cen_idi_ta_2013;
	by snz_uid;
run;

proc freq data=cen_idi_ta_2013;
	tables source lag ave_p_match match*training;
	tables source lag ave_p_match match*training / missing norow nopercent;
run;

proc sort data=project.source_match_bootstrap;
	by snz_uid descending ave_p_match pos_lag neg_lag;
run;

** Now a different approach - should we switch or not. Based on switching to the TA with the next highest prob of being correct, 
** using the following potential decision criteria:
**  1 - How many addresses agree with the first option?
**  2 - How many addresses agree with the first option and are within 0.01?
**  3 - How many addresses agree with the first option and are within 0.05?
**  4 - What is the difference between the probability of the first option and the second option;
**  5 - How many addresses agree with the second option?
**  6 - How many addresses agree with the second option and are within 0.01?
**  7 - How many addresses agree with the second option and are within 0.05?

** Want to maximise the probability a switch would be correct and the original address incorrect;
data allocate;
	length first_ta switch_ta $ 3;
	retain snz_uid first_match switch_match first_source first_lag switch_source switch_lag n first_ta first_p switch_ta switch_p switch first_count switch_count first_count1 switch_count1 first_count5 switch_count5;
	set project.source_match_bootstrap;
	by snz_uid;
	** Split our 50% validation set from before into a new training and validation set - 25% of the total sample each;
	if random <0.25 then training2=0;
	else if random<0.5 then training2=1;
	if first.snz_uid then do;
		n=0;
		first_ta=source_ta;
		first_p=ave_p_match;
		first_source=source;
		first_lag=lag;
		switch_p=.;
		first_count=0;
		first_count1=0;
		first_count5=0;
		switch_count=0;
		switch_count1=0;
		switch_count5=0;
		switch_ta='';
		first_match=match;
		switch_match=0;
		switch=0;
	end;
	if switch=0 and first_ta ne source_ta then do;
		switch_ta=source_ta;
		switch=1;
		switch_p=ave_p_match;
		switch_source=source;
		switch_lag=lag;
		if switch_ta=ta then switch_match=1;
	end;
	if first_ta=source_ta then do;
		first_count+1;
		if abs(first_p-ave_p_match)<=0.01 then first_count1+1;
		if abs(first_p-ave_p_match)<=0.05 then first_count5+1;
	end;
	if switch_ta=source_ta then do;
		switch_count+1;
		if abs(first_p-ave_p_match)<=0.01 then switch_count1+1;
		if abs(first_p-ave_p_match)<=0.05 then switch_count5+1;
	end;
	p_diff=first_p-switch_p;
	if last.snz_uid then output;
run;

** Run another decision tree to decide whether to take the TA with the best probability of being a match, or the one with the second best (i.e. the switch TA)
** based on the 7 criteria outlined above;
proc hpsplit data=allocate;
	class switch_match;
	model switch_match = p_diff first_count switch_count first_count1 switch_count1 first_count5 switch_count5 ;
	partition rolevar=training2(train='1',valid='0');
	code file="&path.\Decision Rules\code_allocate.sas";
	rules file="&path.\Decision Rules\rules_allocate.txt";
	prune costcomplexity;
run;

** Now decide whether to switch or not based on these rules and do the final TA assignment to the 2013 Census records;
data project.cen_idi_ta_2013;
	length source $ 6;
	set allocate;
	%include "&path.\Decision Rules\code_allocate.sas";
	if p_switch_match>0.5 then do;
		pred_ta=switch_ta;
		match=switch_match;
		source=switch_source;
		lag=switch_lag;
		switch=1;
	end;
	else do;
		pred_ta=first_ta;
		match=first_match;
		source=first_source;
		lag=first_lag;
		switch=0;
	end;
run;

** Assess whether our TA assignment matches and add on relevant Census variables;
data project.cen_idi_ta_2013(drop=cen_ind_age_code cen_ind_sex_code);
	merge project.censusaddress(in=a drop=source) project.cen_idi_ta_2013 cen.census_individual(keep=snz_uid cen_ind_age_code cen_ind_sex_code);
	by snz_uid;
	if a;
	if ta=pred_ta then match=1;
	else if pred_ta='' then match=.;
	else match=0;
	age=input(cen_ind_age_code,3.);
	sex=cen_ind_sex_code;
run;

** Check the match for training and validation sets - almost identical match rates;
proc freq data=project.cen_idi_ta_2013;
	tables training*training2*match/missing;
run;