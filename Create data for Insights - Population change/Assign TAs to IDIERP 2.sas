*** Assign TAs to the IDI ERP at each year using rules developed from the 2013 Census - define and describe population change over time, and output the data;

** Start out by cleaning up the work library;
proc datasets lib=work nolist;
	save formats;
quit;
run;

** Identify all potential TA matches for each IDI ERP June year;
%macro idierp_ta(year);
%let month=06;
%let erpdat=JUN&year.;
data project.idierp_&erpdat._ta;
snz_uid=.;
run;

** Bring all potential TAs together for the period of interest for the IDI ERP;
%macro addsourceta(source=);
proc sql;
	create table source_before as
	select a.snz_uid, b.TA as source_TA , b.startdate as source_date, "&source." as source
	%if &source.= moeter %then %do; , b.provider %end; from
	project.idierp_&erpdat. a inner join project.&source.address b
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
	select a.snz_uid, b.TA as source_TA, b.startdate as source_date, "&source." as source
	%if &source.= moeter %then %do; , b.provider %end; from
	project.idierp_&erpdat. a inner join project.&source.address b
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

data project.idierp_&erpdat._ta;
	set project.idierp_&erpdat._ta source;
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
*%addsourceta(source=msdp);
%addsourceta(source=msdr);
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
	select a.snz_uid, b.TA as source_TA, mdy(%eval(&month.+1),01,&year.) as source_date, "emp" as source from
	project.idierp_&erpdat. a inner join project.empaddress b
 	on a.snz_uid=b.snz_uid and b.firstdate <= mdy(%eval(&month.+1),01,&year.) <= b.lastdate
	order by snz_uid, source_date descending;
quit;

data source_overlap;
	set source_overlap;
	by snz_uid;
	if first.snz_uid;
run;

proc sql;
	create table source_before as
	select a.snz_uid, b.TA as source_TA, b.firstdate, b.lastdate, "emp" as source from
	project.idierp_&erpdat. a inner join project.empaddress b
 	on a.snz_uid=b.snz_uid and b.lastdate < mdy(%eval(&month.+1),01,&year.) 
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
	select a.snz_uid, b.TA as source_TA, b.firstdate, b.lastdate, "emp" as source from
	project.idierp_&erpdat. a inner join project.empaddress b
 	on a.snz_uid=b.snz_uid and b.firstdate > mdy(%eval(&month.+1),01,&year.) 
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

data project.idierp_&erpdat._ta;
	set project.idierp_&erpdat._ta source_overlap source2;
	by snz_uid;
	if snz_uid ne .;
run;

** Restrict to the providers that had a good Census TA match;
proc sql;
	create table project.idierp_&erpdat._ta as
	select a.*, b.provider as provider_keep
	from project.idierp_&erpdat._ta a left join project.providers_keep b
	on a.provider = b.provider
	order by snz_uid;
quit;

data project.idierp_&erpdat._ta;
	set project.idierp_&erpdat._ta;
	if source='moeter' and provider ne '' and provider_keep='' then delete;
run;

proc sql;
	create table project.idierp_&erpdat._ta as
	select a.*, b.ta as emp_ta
	from project.idierp_&erpdat._ta a left join project.employers_keep b
	on a.source_ta = b.ta and a.source = 'emp'
	order by snz_uid;
quit;

data project.idierp_&erpdat._ta(drop=emp_ta);
	set project.idierp_&erpdat._ta;
	if emp_ta='' and source='emp' then delete;
run;

** Now we want to look at the time between the collection of TA data and the IDI ERP date;
data project.idierp_&erpdat._ta;
	set project.idierp_&erpdat._ta;
	if source_ta in ('004','005','006','007','008','009','010') then source_ta='076';
	else if source_ta = '061' then source_ta='060';
	startdate=mdy(%eval(&month.+1),01,&year.);
	lag=floor((startdate-source_date)/365.25);
	lagdays=startdate-source_date;
	lagmths=floor((startdate-source_date)/365.25*12);
	if lag>10 or lag<-5 then delete;
run;
%mend idierp_ta;

%idierp_ta(2008);
%idierp_ta(2009);
%idierp_ta(2010);
%idierp_ta(2011);
%idierp_ta(2012);
%idierp_ta(2013);
%idierp_ta(2014);
%idierp_ta(2015);
%idierp_ta(2016);

%include "&path.\assign_location_macro.sas";

** Now choose the most likely TA match for each person in the IDI ERP;
%macro assign_idierp_ta(year);
%let erpdat=JUN&year.;

proc sql;
	create table idierp_&erpdat._ta2 as
	select a.*, b.sex, b.age
	from project.idierp_&erpdat._ta a left join project.idierp_&erpdat. b
	on a.snz_uid = b.snz_uid;
quit;
%include "&path.\assign_location_macro.sas";

%assign_location(indat=idierp_&erpdat._ta2,outdat=project.idierp_&erpdat._ta2);

** Add assigned TAs back onto the full IDI ERP - code TA to 099 if no TA could be allocated;
data project.idierp_&erpdat._ta2;
	merge project.idierp_&erpdat. project.idierp_&erpdat._ta2;
	by snz_uid;
	if pred_ta='' then pred_ta='099';
run;
%mend assign_idierp_ta;

%assign_idierp_ta(2008);
%assign_idierp_ta(2009);
%assign_idierp_ta(2010);
%assign_idierp_ta(2011);
%assign_idierp_ta(2012);
%assign_idierp_ta(2013);
%assign_idierp_ta(2014);
%assign_idierp_ta(2015);
%assign_idierp_ta(2016);

** Output summary statistics across IDI ERP years - combine all years into one set of output;
%macro out_idi;
data idi_all;
run;
%do year=2008 %to 2016;
proc tabulate data=project.idierp_jun&year._ta2 missing out=idi_&year.;
	class lag source age pred_ta sex;
	tables lag,all*N;
	tables source,all*N;
	tables sex*age*source,all*N;
	tables pred_ta,all*N;
	format age age_gp.;
run;

data idi_all(drop=_type_ _page_ _table_);
	retain year lag source sex age pred_ta N;
	set idi_all idi_&year.(in=a);
	if a then year=&year.;
 	if N ne .;
run;
%end;

data idi_all;
	set idi_all;
	if N < 6 then N=0;
run;

%rr3(idi_all,idi_all,N);

** Expect summary stats;
proc export data=idi_all(where=(lag ne . or source ne '' or pred_ta ne '')) replace outfile="&path.\Tables\IDI TA assignment by lag source and ta.csv" dbms=csv;
format pred_ta $ta.;
run;
%mend out_idi;

%out_idi;

** Now we want to define changes in the TA population between any two periods as being due to internal migration, international migration, or natural increase;
** We also want to describe these flows according to various characteristics, and output in a single table for the data visualisation;

** Firstly identify people who are overseas according to our 12/16 criteria in each year;
%macro overseas_yr(year);
%let prevyear = %eval(&year. - 1);
%let nextyear = %eval(&year. + 1);

proc sql;
   create table overseas_spells_1yr_&year. as
   select distinct snz_uid , pos_applied_date, pos_ceased_date, pos_day_span_nbr
   from data.person_overseas_spell
   where pos_applied_date <= "28FEB&nextyear.:23:59:59.999"dt and pos_ceased_date >= "01NOV&prevyear.:00:00:00.000"dt
   order by snz_uid, pos_applied_date;
quit;

** Calculate number of days spent overseas **;
data overseas_time_1yr_&year.;
   set overseas_spells_1yr_&year.;
   if pos_ceased_date > "28FEB&nextyear.:23:59:59.999"dt and pos_applied_date < "01NOV&prevyear.:00:00:00.000"dt 
      then time_to_add = 365;

   else if pos_ceased_date > "28FEB&nextyear.:23:59:59.999"dt  and pos_applied_date >= "01NOV&prevyear.:00:00:00.000"dt
      then time_to_add = ("28FEB&nextyear.:23:59:59.999"dt - pos_applied_date) / 86400;

   else if pos_ceased_date <= "28FEB&nextyear.:23:59:59.999"dt and pos_applied_date >= "01NOV&prevyear.:00:00:00.000"dt 
      then time_to_add = (pos_ceased_date - pos_applied_date) / 86400;

   else if pos_ceased_date <= "28FEB&nextyear.:23:59:59.999"dt and pos_applied_date < "01NOV&prevyear.:00:00:00.000"dt 
      then time_to_add = (pos_ceased_date - "01NOV&prevyear.:00:00:00.000"dt) / 86400;
run;

proc sql;
   create table time_overseas_1yr_&year. as 
   select snz_uid, ROUND(SUM(time_to_add),1) as days_overseas_last1
   from overseas_time_1yr_&year.
   group by snz_uid;
quit;

data project.out_&year.(keep=snz_uid out_&year.);
	set time_overseas_1yr_&year.;
	if days_overseas_last1 > 122 then out_&year.=1;
	else out_&year.=0;
run;
%mend overseas_yr;

%overseas_yr(2008);
%overseas_yr(2009);
%overseas_yr(2010);
%overseas_yr(2011);
%overseas_yr(2012);
%overseas_yr(2013);
%overseas_yr(2014);
%overseas_yr(2015);
%overseas_yr(2016);

** Now for each combination of June years we want to see where each person was in June of the other year;
** Either IDI ERP, or overseas, or not alive;

** First select first recorded arrivals in NZ - as long as they didn't have a departure record before it;
proc sql;
	create table first_arrives as
	select distinct snz_uid, min(input(dol_mov_carrier_date,yymmdd10.)) as first_arrive_date format yymmdd10., dol_mov_nationality_code as nationality
	from dol.movements(where=(dol_mov_movement_ind='A'))
	group by snz_uid
	order by snz_uid;
quit;

proc sql;
	create table first_departs as
	select distinct snz_uid, min(input(dol_mov_carrier_date,yymmdd10.)) as first_depart_date format yymmdd10.
	from dol.movements(where=(dol_mov_movement_ind='D'))
	group by snz_uid
	order by snz_uid;
quit;

data first_arrives;
	merge first_arrives(in=a) first_departs(in=b);
	by snz_uid;
	if a and (b=0 or first_arrive_date < first_depart_date);
run;

%macro IDI_pop_change(prev_year,curr_year);
** Combine the two years being compared into a single dataset;
proc sql;
	create table project.pop_jun&prev_year._jun&curr_year. as
	select coalesce(a.snz_uid,b.snz_uid) as snz_uid, coalesce(a.sex,b.sex) as sex, 
			"&prev_year." as prev_year, a.pred_ta as prev_ta,
			"&curr_year." as curr_year, b.pred_ta as curr_ta
	from project.idierp_jun&prev_year._ta2 a full join project.idierp_jun&curr_year._ta2 b
	on a.snz_uid=b.snz_uid;
quit;

** Create birth and death indicators and extract ethnicity data;
proc sql;
	create table project.pop_jun&prev_year._jun&curr_year. as
	select a.*, b.snz_birth_year_nbr as birthyear, b.snz_birth_month_nbr as birthmonth, b.snz_deceased_year_nbr as deathyear, b.snz_deceased_month_nbr as deathmonth
	from project.pop_jun&prev_year._jun&curr_year. a left join data.personal_detail b
	on a.snz_uid = b.snz_uid;
quit;

proc sql;
	create table project.pop_jun&prev_year._jun&curr_year. as
	select a.*, b.snz_ethnicity_grp1_nbr as ethnic1, b.snz_ethnicity_grp2_nbr as ethnic2, b.snz_ethnicity_grp3_nbr as ethnic3, b.snz_ethnicity_grp4_nbr as ethnic4, 
			b.snz_ethnicity_grp5_nbr as ethnic5, b.snz_ethnicity_grp6_nbr as ethnic6
	from project.pop_jun&prev_year._jun&curr_year. a left join data.source_ranked_ethnicity b
	on a.snz_uid = b.snz_uid;
quit;

data project.pop_jun&prev_year._jun&curr_year.;
	set project.pop_jun&prev_year._jun&curr_year.;
	if (birthmonth>6 and birthyear=&prev_year.) or (birthyear>&prev_year. and birthyear<&curr_year.) or (birthmonth<=6 and birthyear=&curr_year.)
		then birth=1;
	else birth=0;
	if (deathmonth>6 and deathyear=&prev_year.) or (deathyear>&prev_year. and deathyear<&curr_year.) or (deathmonth<=6 and deathyear=&curr_year.)
		then death=1;
	else death=0;
	birthdt=mdy(birthmonth,15,birthyear);
	deathdt=mdy(deathmonth,15,deathyear);
	format birthdt deathdt date9.;
	** Create an unknown ethnicity field;
	if sum(of ethnic1-ethnic6)<1 then ethnic7=1;
	array eth{*} ethnic1-ethnic7;
	do i=1 to dim(eth);
		if eth{i}=0 then eth{i}=.;
	end;
run;

** Now add an indicator if overseas more than 4 months of the 16 month period centred on the other year if not in the IDI ERP in that year;
** Later we will differentiate migrants (early/recent) from NZ born - plus country of origin;
data project.pop_jun&prev_year._jun&curr_year.;
	merge project.pop_jun&prev_year._jun&curr_year.(in=a) project.out_&curr_year.(in=b) project.out_&prev_year.(in=c);
	by snz_uid;
	if a;
	if b then departure=1;
	else departure=0;
	if c then arrival=1;
	else arrival=0;
run;

** If in one ERP but not the other then define the change as being due to birth, death, arrival, departure or unknown (999);
data project.pop_jun&prev_year._jun&curr_year.;
	set project.pop_jun&prev_year._jun&curr_year.;
	if curr_ta='' then do;
		if death then curr_ta='000';
		else if departure then curr_ta='900';
		else curr_ta='999';
	end;
	if prev_ta='' then do;
		if birth then prev_ta='000';
		else if arrival then prev_ta='900';
		else prev_ta='999';
	end;
run;

** Now characterise each person's status looking forward from the first period to the second and looking back from the second to the first;
data project.pop_jun&prev_year._jun&curr_year.;
	set project.pop_jun&prev_year._jun&curr_year.;
	** Forward status 1 - same TA; 
	if '001' le prev_ta le '076' and curr_ta=prev_ta then status_fwd=1;
	** Forward status 2 - moved TA; 
	else if '001' le prev_ta le '076' and '001' le curr_ta le '076' then status_fwd=2;
	** Forward status 3 - left NZ; 
	else if '001' le prev_ta le '076' and curr_ta ='900' then status_fwd=3;
	** Forward status 4 - died;
	else if '001' le prev_ta le '076' and curr_ta ='000' then status_fwd=4;
	** Forward status 5 - moved to unknown area;
	else if '001' le prev_ta le '076' and curr_ta ='099' then status_fwd=5;
	** Forward status 6 - in NZ but not in ERP; 
	else if '001' le prev_ta le '076' and curr_ta ='999' then status_fwd=6;
	** Forward status 9 - unknown location at start; 
	else if prev_ta='099' then status_fwd=9;

	** Back status 1 - same TA; 
	if '001' le curr_ta le '076' and curr_ta=prev_ta then status_bak=1;
	** Back status 2 - moved TA; 
	else if '001' le curr_ta le '076' and '001' le prev_ta le '076' then status_bak=2;
	** Back status 3 - arrived in NZ; 
	else if '001' le curr_ta le '076' and prev_ta ='900' then status_bak=3;
	** Back status 4 - born;
	else if '001' le curr_ta le '076' and prev_ta ='000' then status_bak=4;
	** Back status 5 - moved from unknown area;
	else if '001' le curr_ta le '076' and prev_ta ='099' then status_bak=5;
	** Back status 6 - in NZ but not in ERP; 
	else if '001' le curr_ta le '076' and prev_ta ='999' then status_bak=6;
	** Back status 9 - unknown location at end;
	else if curr_ta='099' then status_bak=9;
	format status_fwd fwd_cat. status_bak bak_cat.;
	label prev_ta="TA &prev_year." curr_ta="TA &curr_year." status_fwd="Outflows &prev_year. to &curr_year." status_bak="Inflows &prev_year. to &curr_year.";
run;

** Add on country of origin from first arrivals;
proc sql;
	create table project.pop_jun&prev_year._jun&curr_year. as
	select a.*, b.nationality as country, b.first_arrive_date
	from project.pop_jun&prev_year._jun&curr_year. a left join first_arrives b
	on a.snz_uid = b.snz_uid
	order by snz_uid, first_arrive_date;
quit;

** Add on visa category from the most recent visa issued before the end of December of the second year of interest;
** This relies on MBIE having run an updated migration spells dataset in the DOL sandpit;
proc sql;
	create table project.pop_jun&prev_year._jun&curr_year. as
	select a.*, b.spell_substream, b.start_date, b.end_date, b.res_year, b.res_month
	from project.pop_jun&prev_year._jun&curr_year. a left join sanddol.migration_spells(where=(start_date<="31Dec&curr_year."d)) b
	on a.snz_uid = b.snz_uid
	order by snz_uid, start_date;
quit;

data project.pop_jun&prev_year._jun&curr_year.(drop=start_date end_date);
	length visa_cat $ 3;
	set project.pop_jun&prev_year._jun&curr_year.;
	by snz_uid;
	if last.snz_uid;
	country_det=country;

	** Categorise as a New Zealander (visa_cat=NA) if they were first granted residence more than 10 years before the current June year;
	if res_year ne . and (res_year < %eval(&curr_year.-10) or (res_year = %eval(&curr_year.-10) and res_month<=6)) then visa_cat='NA';
	else if spell_substream='Res-Unknown' then visa_cat='NA';
	** Also categorise as a New Zealander (visa_cat=NA) if they were an Australian who first arrived more than 10 years before the current June year;
	** This could be better refined - perhaps better to leave as Australian since they may have just visited in the earlier period but leave for now;
	else if country='AU' and first_arrive_date<="30JUN%eval(&curr_year.-10)"d then visa_cat='NA';
	else if country='AU' then visa_cat='AUS';

	** Categorise countries into regions;
	if country='' or visa_cat='NA' then country='NZ';
	else if country not in ('AU','CN','IN','GB','PH','ZA','FJ','KR','TO','WS','US','MY','NL','DE','JP','LK','IE','CA','TH','TW','BR','HK','RU','VN','FR') then do;
		if country in ('AQ','AS','CK','FM','GU','KI','MH','MP','NC','NF','NR','NU','PF','PG','PN','PW','SB','TK','TV','VU','WF') then country='91';  *Oceania*;
		else if country in ('AT','BE','CH','DD','DK','FI','FO','GL','IS','LI','LU','MC','NO','SE') then country='92'; *NW Europe;
		else if country in ('AD','AL','BA','BG','BY','CS','CY','CZ','EE','ES','GI','GR','HR','HU','IT','LT','LV','MD','ME','MK','MT','PL','PT','RK','RO','RS','SI','SK','SM','SU','SX','UA','VA','YM','ZZ') then country='93'; *Southern and Eastern Europe;
		else if country in ('AE','BH','DZ','EG','EH','IL','IQ','IR','JO','KW','LB','LY','MA','OM','PS','PX','QA','SA','SD','SY','TN','TR','YD','YE') then country='94'; * Nth Africa and Middle East;
		else if country in ('BN','BU','ID','KH','LA','MM','SG','TL','TP','KP','MN','MO') then country='95'; * Nth-East and South East Asia;
		else if country in ('AF','AM','AZ','BD','BT','GE','KG','KZ','MV','NP','PK','TJ','TM','UZ') then country='96'; * Southern and Central Asia;
		else if country in ('AG','AI','AR','AW','BB','BM','BO','BS','BZ','CL','CO','CR','CU','DM','DO','EC','FK','GD','GF','GP','GT','GY','HN',
				'HT','JM','KN','KY','LC','MQ','MS','MX','NI','PA','PE','PM','PR','PY','SR','SV','TC','TT','UY','VC','VE','VG','VI') then country='97'; * The Americas;
		else if country in ('AO','BF','BI','BJ','BW','CD','CF','CG','CI','CM','CV','DJ','ER','ET','GA','GH','GM','GN','GQ','GW','KE','KM','LR','LS','MG','ML','MR',
			'MU','MW','MZ','NA','NE','NG','RE','RW','SC','SH','SL','SN','SO','ST','SZ','TD','TG','TZ','UG','YT','ZM','ZR','ZW') then country='98'; * Sub-Saharan Africa;
		else country='ZZ';
	end;

	** Categorise visas into broad categories;
	if spell_substream='' and country='NZ' then visa_cat='NA';
	else if substr(spell_substream,1,3)='Res' then visa_cat='RES';
	else if substr(spell_substream,1,7)='Student' then visa_cat='STU';
	else if substr(spell_substream,1,4)='Work' or substr(spell_substream,1,5)='Study' then visa_cat='WRK';
	else if spell_substream not in ('NA','AUS') then visa_cat='OTH';

	if country='NZ' then visa_cat = 'NA';

	** Re-code births as external migrants if they were born before they first arrived in NZ;
	if mdy(birthmonth,1,birthyear) < first_arrive_date and status_bak=4 then status_bak=3;

	** Create a broader country classification for looking at flows that do not relate to international migration - just NZ and other;
	if country='NZ' then country_nz='NZ';
	else country_nz='AA';
	format prev_ta curr_ta $3.;
run;

** Calculate age at both periods of interest;
data project.pop_jun&prev_year._jun&curr_year.;
	set project.pop_jun&prev_year._jun&curr_year.;
   	prev_age = &prev_year. - birthyear;
   	if birthmonth > 6 then prev_age = prev_age - 1;
	if prev_age<0 then prev_age=.;
   	curr_age = &curr_year. - birthyear;
   	if birthmonth > 6 then curr_age = curr_age - 1;
	if curr_age<0 then curr_age=.;
	format curr_age prev_age age_gp.;
run;
%mend IDI_pop_change;

** 1 year changes;
%idi_pop_change(2015,2016);
%idi_pop_change(2014,2015);
%idi_pop_change(2013,2014);
%idi_pop_change(2012,2013);
%idi_pop_change(2011,2012);
%idi_pop_change(2010,2011);
%idi_pop_change(2009,2010);
%idi_pop_change(2008,2009);

** 2 year changes;
%idi_pop_change(2014,2016);
%idi_pop_change(2013,2015);
%idi_pop_change(2012,2014);
%idi_pop_change(2011,2013);
%idi_pop_change(2010,2012);
%idi_pop_change(2009,2011);
%idi_pop_change(2008,2010);

** 3 year changes;
%idi_pop_change(2013,2016);
%idi_pop_change(2012,2015);
%idi_pop_change(2011,2014);
%idi_pop_change(2010,2013);
%idi_pop_change(2009,2012);
%idi_pop_change(2008,2011);

** 4 year changes;
%idi_pop_change(2012,2016);
%idi_pop_change(2011,2015);
%idi_pop_change(2010,2014);
%idi_pop_change(2009,2013);
%idi_pop_change(2008,2012);

** 5 year changes;
%idi_pop_change(2011,2016);
%idi_pop_change(2010,2015);
%idi_pop_change(2009,2014);
%idi_pop_change(2008,2013);

** 6 year changes;
%idi_pop_change(2010,2016);
%idi_pop_change(2009,2015);
%idi_pop_change(2008,2014);

** 7 year changes;
%idi_pop_change(2009,2016);
%idi_pop_change(2008,2015);

** 8 year changes;
%idi_pop_change(2008,2016);

data project.popchange_summary;
run;
** Now output summary statistics in a single table;
%macro output_idi_pop_change(prev_year,curr_year);
** First summarise total flows (in and out by category) by TA;
proc freq data=project.pop_jun&prev_year._jun&curr_year. noprint;
	tables prev_ta*status_fwd /missing out=popoutflow_&prev_year._&curr_year.(drop=percent);
run;

data popoutflow_&prev_year._&curr_year.;
	length status $ 20;
	set popoutflow_&prev_year._&curr_year.(rename=(prev_ta=ta));
	ta_code=ta;
	status=put(status_fwd,fwd_cat.);
	format status_fwd 8.;
	prev_year="&prev_year.";
	curr_year="&curr_year.";
	format ta $ta. curr_age prev_age age_gp.;
run;

proc transpose data=popoutflow_&prev_year._&curr_year. out=popoutflow_&prev_year._&curr_year.(drop=_name_ _label_) prefix=out_;
	id status_fwd;
	by ta_code ta prev_year curr_year;
	idlabel status;
	var count;
	where '001' le ta_code le '099';
run;

proc freq data=project.pop_jun&prev_year._jun&curr_year. noprint;
	tables curr_ta*status_bak /missing out=popinflow_&prev_year._&curr_year.(drop=percent);
run;

data popinflow_&prev_year._&curr_year.;
	length status $ 20;
	set popinflow_&prev_year._&curr_year.(rename=(curr_ta=ta));
	ta_code=ta;
	status=put(status_bak,bak_cat.);
	format status_bak 8.;
	prev_year="&prev_year.";
	curr_year="&curr_year.";
	format ta $ta. curr_age prev_age age_gp.;
run;

proc transpose data=popinflow_&prev_year._&curr_year. out=popinflow_&prev_year._&curr_year.(drop=_name_ _label_) prefix=in_;
	id status_bak;
	by ta_code ta prev_year curr_year;
	idlabel status;
	var count;
	where '001' le ta_code le '099';
run;

data popflow_&prev_year._&curr_year.;
	retain ta_code ta prev_year curr_year out_1-out_5 out_9 total_prev in_2-in_5 in_9 total_curr;
	merge popoutflow_&prev_year._&curr_year. popinflow_&prev_year._&curr_year.;
	by ta_code ta prev_year curr_year;
	total_prev=sum(of out_1-out_5,out_9);
	total_curr=sum(out_1,of in_2-in_5,in_9);
run;

** Now do specific subflows;
%macro subflow(whereflow,wherevar,out,in);
%if &out.=y %then %do;
proc freq data=project.pop_jun&prev_year._jun&curr_year. noprint;
	tables prev_ta*&wherevar. /missing out=out_&whereflow._&wherevar._&prev_year._&curr_year.(drop=percent);
	%if &whereflow.=tot %then %do; where status_fwd in (1,2,3,4,5); %end;
	%else %do; where status_fwd=&whereflow.; %end;
run;

data out_&whereflow._&wherevar._&prev_year._&curr_year.;
	set out_&whereflow._&wherevar._&prev_year._&curr_year.(rename=(prev_ta=ta));
	ta_code=ta;
	status_fwd="&whereflow.";
	%if &wherevar.=curr_ta %then %do; format curr_ta $3.; %end;
	prev_year="&prev_year.";
	curr_year="&curr_year.";
	format ta $ta. curr_age prev_age age_gp.;
run;

proc transpose data=out_&whereflow._&wherevar._&prev_year._&curr_year. out=out_&whereflow._&wherevar._&prev_year._&curr_year.(drop=_name_ _label_) prefix=out_&whereflow._&wherevar._;
	id &wherevar.;
	by ta_code ta prev_year curr_year;
	var count;
	where '001' le ta_code le '099';
run;

data popflow_&prev_year._&curr_year.;
	merge popflow_&prev_year._&curr_year. out_&whereflow._&wherevar._&prev_year._&curr_year.;
	by ta_code ta prev_year curr_year;
run;
%end;

%if &in.=y %then %do;
proc freq data=project.pop_jun&prev_year._jun&curr_year. noprint;
	tables curr_ta*&wherevar./missing out=in_&whereflow._&wherevar._&prev_year._&curr_year.(drop=percent);
	%if &whereflow.=tot %then %do; where status_bak in (1,2,3,4,5); %end;
	%else %do; where status_bak=&whereflow.; %end;
run;

data in_&whereflow._&wherevar._&prev_year._&curr_year.;
	set in_&whereflow._&wherevar._&prev_year._&curr_year.(rename=(curr_ta=ta));
	ta_code=ta;
	status_bak="&whereflow.";
	%if &wherevar.=prev_ta %then %do; format prev_ta $3.; %end;
	prev_year="&prev_year.";
	curr_year="&curr_year.";
	format ta $ta. curr_age prev_age age_gp.;
run;

proc transpose data=in_&whereflow._&wherevar._&prev_year._&curr_year. out=in_&whereflow._&wherevar._&prev_year._&curr_year.(drop=_name_ _label_) prefix=in_&whereflow._&wherevar._;
	id &wherevar.;
	by ta_code ta prev_year curr_year;
	var count;
	where '001' le ta_code le '099';
run;

data popflow_&prev_year._&curr_year.;
	merge popflow_&prev_year._&curr_year. in_&whereflow._&wherevar._&prev_year._&curr_year.;
	by ta_code ta prev_year curr_year;
run;
%end;
%mend subflow;

** Now break down flows by age, sex, ethnicity, country of origin, and migrant category;
%subflow(1,prev_age,y,n);
%subflow(1,curr_age,y,n);
%subflow(1,sex,y,n);
%subflow(1,ethnic1,y,n);
%subflow(1,ethnic2,y,n);
%subflow(1,ethnic3,y,n);
%subflow(1,ethnic4,y,n);
%subflow(1,ethnic5,y,n);
%subflow(1,ethnic6,y,n);
%subflow(1,ethnic7,y,n);
%subflow(1,country_nz,y,n);
%subflow(2,prev_age,y,y);
%subflow(2,curr_age,y,y);
%subflow(2,sex,y,y);
%subflow(2,ethnic1,y,y);
%subflow(2,ethnic2,y,y);
%subflow(2,ethnic3,y,y);
%subflow(2,ethnic4,y,y);
%subflow(2,ethnic5,y,y);
%subflow(2,ethnic6,y,y);
%subflow(2,ethnic7,y,y);
%subflow(2,country_nz,y,y);
%subflow(2,curr_ta,y,n);
%subflow(2,prev_ta,n,y);
%subflow(3,prev_age,y,y);
%subflow(3,curr_age,y,y);
%subflow(3,sex,y,y);
%subflow(3,ethnic1,y,y);
%subflow(3,ethnic2,y,y);
%subflow(3,ethnic3,y,y);
%subflow(3,ethnic4,y,y);
%subflow(3,ethnic5,y,y);
%subflow(3,ethnic6,y,y);
%subflow(3,ethnic7,y,y);
%subflow(3,country,y,y);
%subflow(3,visa_cat,y,y);
%subflow(4,prev_age,y,y);
%subflow(4,curr_age,y,y);
%subflow(4,sex,y,y);
%subflow(4,ethnic1,y,y);
%subflow(4,ethnic2,y,y);
%subflow(4,ethnic3,y,y);
%subflow(4,ethnic4,y,y);
%subflow(4,ethnic5,y,y);
%subflow(4,ethnic6,y,y);
%subflow(4,ethnic7,y,y);
%subflow(4,country_nz,y,y);
%subflow(tot,prev_age,y,y);
%subflow(tot,curr_age,y,y);
%subflow(tot,sex,y,y);
%subflow(tot,ethnic1,y,y);
%subflow(tot,ethnic2,y,y);
%subflow(tot,ethnic3,y,y);
%subflow(tot,ethnic4,y,y);
%subflow(tot,ethnic5,y,y);
%subflow(tot,ethnic6,y,y);
%subflow(tot,ethnic7,y,y);
%subflow(tot,country_nz,y,y);

proc means data=popflow_&prev_year._&curr_year. noprint;
	var in: out: total:;
	output sum= out=total_flow(drop=_:);
run;

data project.popchange_summary;
	set project.popchange_summary popflow_&prev_year._&curr_year. total_flow(in=tot);
	if _n_=1 and ta_code='' then delete;
	if tot then do; ta_code='999'; ta='999'; 	prev_year="&prev_year."; curr_year="&curr_year."; end;
run;
%mend output_idi_pop_change;

** 1 year changes;
%output_idi_pop_change(2015,2016);
%output_idi_pop_change(2014,2015);
%output_idi_pop_change(2013,2014);
%output_idi_pop_change(2012,2013);
%output_idi_pop_change(2011,2012);
%output_idi_pop_change(2010,2011);
%output_idi_pop_change(2009,2010);
%output_idi_pop_change(2008,2009);

** 2 year changes;
%output_idi_pop_change(2014,2016);
%output_idi_pop_change(2013,2015);
%output_idi_pop_change(2012,2014);
%output_idi_pop_change(2011,2013);
%output_idi_pop_change(2010,2012);
%output_idi_pop_change(2009,2011);
%output_idi_pop_change(2008,2010);

** 3 year changes;
%output_idi_pop_change(2013,2016);
%output_idi_pop_change(2012,2015);
%output_idi_pop_change(2011,2014);
%output_idi_pop_change(2010,2013);
%output_idi_pop_change(2009,2012);
%output_idi_pop_change(2008,2011);

** 4 year changes;
%output_idi_pop_change(2012,2016);
%output_idi_pop_change(2011,2015);
%output_idi_pop_change(2010,2014);
%output_idi_pop_change(2009,2013);
%output_idi_pop_change(2008,2012);

** 5 year changes;
%output_idi_pop_change(2011,2016);
%output_idi_pop_change(2010,2015);
%output_idi_pop_change(2009,2014);
%output_idi_pop_change(2008,2013);

** 6 year changes;
%output_idi_pop_change(2010,2016);
%output_idi_pop_change(2009,2015);
%output_idi_pop_change(2008,2014);

** 7 year changes;
%output_idi_pop_change(2009,2016);
%output_idi_pop_change(2008,2015);

** 8 year changes;
%output_idi_pop_change(2008,2016);

** Apply random rounding and suppression;
%macro suppress_rr3(indata=);
%let indata=project.popchange_summary;
proc contents data=&indata. out=vars(keep=name type) noprint;
run;

proc sort data=vars;
	by name;
run;

data vars;
	set vars;
	where type=1 /* need to also exclude any numeric variables here that you don't want to be rounded */;
	i+1;
	call symput('var'||trim(left(put(i,8.))),trim(name));
	call symput('numvars',_n_);
run;

data round_rr3;
	set &indata.;
run;

%do j=1 %to &numvars.;
	** First suppress unrounded cells of less than 6 (replace with missing value);
	data round_rr3;
		set round_rr3;
		if &&var&j. < 6 then &&var&j.=0;
	run;
	%rr3(round_rr3,round_rr3,&&var&j.);
%end;

** Create rounded dataset with rounded vars in alphabetical order;
data &indata._rr3;
	retain ta_code ta prev_year curr_year %do j=1 %to &numvars.; &&var&j. %end; ;
	set round_rr3;
	** Replace in_1 with the same value as out_1 as they are the same and should be rounded the same way;
	in_1 = out_1;
run;
%mend suppress_rr3;

%suppress_rr3(indata=project.popchange_summary);
data _null_;
	today=put(today(),yymmddn8.);
	call symput('today',today);
run;

%put &today;

proc export data=project.popchange_summary_rr3 replace outfile="&path.\tables\idi ta population change &today..csv" dbms=csv;run;

data idi_age;
run;

** Produce an age breakdown of the IDI ERP across all years - just from age 0 to 100;
%macro idi_pop_age(year);
proc freq data=project.idierp_jun&year._ta2 noprint;
	tables age/out=age&year.(drop=percent);
	where 0<=age<=100;
run;

data idi_age;
	length year $ 4;
	retain year age count;
	set idi_age age&year.(in=b);
	if age=. then delete;
	if b then year="&year.";
run;
%mend idi_pop_age;

%idi_pop_age(year=2008);
%idi_pop_age(year=2009);
%idi_pop_age(year=2010);
%idi_pop_age(year=2011);
%idi_pop_age(year=2012);
%idi_pop_age(year=2013);
%idi_pop_age(year=2014);
%idi_pop_age(year=2015);
%idi_pop_age(year=2016);

ods listing close;
ods html close;
data idi_age;
	set idi_age;
	if count<6 then count=0;
run;

%rr3(idi_age,idi_age,count);
ods excel file="&path.\Tables\IDI population by age.xlsx";
proc print data=idi_age;
run;
ods excel close;
ods html;
ods listing;
