** Construct the various location datasets at TA level - import the data and format the various datasets;

** Macro Chris Ball developed to read in address data - adapted here to just read in TAs (also region but this is not used);
%macro Address_Import(Out = , In = , Prefix = , Source = , indate = );
Proc SQL;
	Connect to odbc (dsn="IDI_Clean_&version._srvprd"); 
	CREATE TABLE &Out. AS 
	SELECT snz_uid, input(StartDate,yymmdd10.) as StartDate format yymmdd10., 
			Region, TA, &Source. as Source %if &source.= 'ter' %then %do; , moe_enr_provider_code as provider %end;
	FROM connection to  odbc (
		select distinct snz_uid, &indate. as StartDate, &Prefix._region_code as Region, &Prefix._ta_code as TA 
			%if &source.= 'ter' %then %do; , moe_enr_provider_code %end;
		from &In.
		where &Prefix._ta_code is not NULL and &indate. is not NULL
		order by snz_uid);
	DISCONNECT FROM odbc;
Quit;
%mend Address_Import;

%Address_Import(Out = project.NHIAddress, In = moh_clean.pop_cohort_nhi_address, Prefix = moh_nhi, Source = 'nhi', indate = moh_nhi_effective_date);
%Address_Import(Out = project.PHOAddress, In = moh_clean.pop_cohort_pho_address, Prefix = moh_adr, Source = 'pho', indate = coalesce(moh_adr_consultation_date,moh_adr_enrolment_date));
%Address_Import(Out = project.MOEAddress, In = moe_clean.student_per, Prefix = moe_spi, Source = 'moe', indate = moe_spi_mod_address_date);
%Address_Import(Out = project.IRDAddress, In = ir_clean.ird_addresses, Prefix = ir_apc, Source = 'ird', indate = ir_apc_applied_date);
%Address_Import(Out = project.ACCAddress, In = acc_clean.claims, Prefix = acc_cla, Source = 'acc', indate = coalesce(acc_cla_lodgement_date, acc_cla_registration_date,acc_cla_accident_date));
%Address_Import(Out = project.MSDRAddress, In = msd_clean.msd_residential_location, Prefix = msd_rsd, Source = 'msdr', indate = msd_rsd_start_date);

%Address_Import(Out = project.MSDPAddress, In = msd_clean.msd_postal_location, Prefix = msd_pst, Source = 'msdp', indate = msd_pst_start_date);

** Now other sources of TA - MOE tertiary, drivers licence;
%Address_Import(Out = project.MOETERAddress, In = moe_clean.enrolment, Prefix = moe_enr_study, Source = 'ter', indate = moe_enr_prog_start_date);
%Address_Import(Out = project.DLRAddress, In = nzta_clean.drivers_licence_register, Prefix = nzta_dlr, Source = 'dlr', indate = nzta_dlr_licence_issue_date);

** Now motor vehicle licensing;
** Current version;
Proc SQL;
	Connect to odbc (dsn="IDI_Clean_&version._srvprd"); 
	CREATE TABLE current_mvr AS 
	SELECT snz_uid, snz_nzta_uid, input(StartDate,yymmdd10.) as StartDate format yymmdd10., TA, 'mvr' as Source
	FROM connection to  odbc (
		select distinct snz_uid, snz_nzta_uid, nzta_mvr_start_date as StartDate, nzta_mvr_ta_code as TA 
		from nzta_clean.motor_vehicle_register
		where nzta_mvr_ta_code is not NULL and nzta_mvr_start_date is not NULL
		order by snz_uid);
	DISCONNECT FROM odbc;
Quit;

proc sort data=current_mvr;
	by snz_nzta_uid snz_uid ;
run;

proc sort data=current_mvr out=mvr_uids(keep=snz_uid snz_nzta_uid) nodupkey;
	by snz_uid snz_nzta_uid;
run;

** And extract and append earlier versions - this is necessary since this data is just a current snapshot and doesn't include historical records;
%macro mvr_old(refresh);
libname nzta_old ODBC dsn=idi_clean_&refresh._srvprd schema=nzta_clean;
Proc SQL;
	CREATE TABLE mvr_&refresh. AS 
	SELECT distinct a.snz_uid, b.snz_nzta_uid, input(b.nzta_mvr_start_date,yymmdd10.) as StartDate format yymmdd10., b.nzta_mvr_ta_code as TA, 'mvr' as Source
	from mvr_uids a left join nzta_old.motor_vehicle_register b
		on a.snz_nzta_uid = b.snz_nzta_uid
	  	where b.nzta_mvr_ta_code is not NULL and b.nzta_mvr_start_date is not NULL
		order by snz_uid, snz_nzta_uid, startdate;
Quit;
%mend mvr_old;

** No TA in the first MVR data supply so exclude 20160715;
*%mvr_old(20160715);
%mvr_old(20161020);
%mvr_old(20161020);
%mvr_old(20170720);

data project.MVRAddress;
	set current_mvr(in=a) mvr_20170720(in=b) mvr_20161020(in=c) mvr_20161020(in=d);
	if a then source=1;
	else if b then source=2;
	else if c then source=3;
	else if d then source=4;
run;

** And dedupe;
proc sort data=project.MVRAddress nodupkey;
	by snz_uid snz_nzta_uid startdate ta;
run;

libname nzta_old ODBC dsn=idi_clean_20160715_srvprd schema=nzta_clean;
data mvr_old;
	set nzta_old.motor_vehicle_register;
	startyear=substr(nzta_mvr_start_date,1,4);
run;

** Now Housing New Zealand data - although this is poor quality and not used in the end;
%macro address_Import_HNZ(Out = , In = , Prefix = , Source = , indate = );
Proc SQL;
	Connect to odbc (dsn="IDI_Clean_&version._srvprd"); 
	CREATE TABLE &Out. AS 
	SELECT snz_uid, datepart(StartDate) as StartDate format yymmdd10., Region, TA, &Source. as Source
	FROM connection to  odbc (
		select distinct b.snz_uid, a.&indate as StartDate, a.&Prefix._region_code as Region, a.&Prefix._ta_code as TA
		from &In. A LEFT JOIN (select snz_uid, snz_msd_uid from security.concordance where snz_msd_uid is not NULL) B
		on A.snz_msd_uid = B.snz_msd_uid
		where &Prefix._ta_code is not NULL and &indate. is not NULL
		order by snz_uid, StartDate
	);
	DISCONNECT FROM odbc ;
Quit;
%mend Address_Import_HNZ;

%Address_Import_HNZ(Out = project.HNZNAddress, In = hnz_clean.new_applications, Prefix = hnz_na, Source = 'hna', indate = hnz_na_date_of_application_date);
%Address_Import_HNZ(Out = project.HNZTAddress, In = hnz_clean.transfer_applications, Prefix = hnz_ta, Source = 'hnt', indate = hnz_ta_application_date);
%Address_Import_HNZ(Out = project.HNZRAddress, In = hnz_clean.register_snapshot, Prefix = hnz_rs, Source = 'hnr', indate = hnz_rs_snapshot_date);

** Now look at the school location as a predictor of student location;
Proc SQL;
	Connect to odbc (dsn="IDI_Clean_&version._srvprd"); 
	CREATE TABLE schoolenrol AS 
	SELECT snz_uid, input(StartDate,yymmdd10.) as StartDate format yymmdd10., 'MOESCH' as Source, input(SchoolNumber,4.) as SchoolNumber
	FROM connection to  odbc (
		select distinct snz_uid, moe_esi_start_date as StartDate, moe_esi_provider_code as SchoolNumber
		from moe_clean.student_enrol
		where moe_esi_provider_code is not NULL and moe_esi_start_date is not NULL
		order by snz_uid);
	DISCONNECT FROM odbc ;
Quit;

** Add on TA of school;
proc sql;
	create table MOESCHAddress1 as
	select a.snz_uid, a.startdate, a.source, b.territorialauthority
	from schoolenrol a inner join metadata.moe_school_profile b
	on a.schoolnumber = b.schoolnumber;
quit;

** Now convert TA text to TA code;
proc sql;
	create table project.MOESCHAddress as
	select a.snz_uid, a.startdate, a.source, a.territorialauthority, b.cat_code as ta
	from MOESCHAddress1 a left join metadata.cen_ta13 b
	on a.territorialauthority = b.descriptor_text
	order by snz_uid, startdate;
quit;

data project.MOESCHAddress(drop=territorialauthority);
	set project.MOESCHAddress;
	if territorialauthority in ('Auckland City', 'Franklin District', 'Manukau City', 'Rodney District') then ta='076';
	if territorialauthority='Tauranga District' then ta='023';
	where territorialauthority ne 'Unknown';
run;

* Household Economic Survey;
proc sql;
	Connect to odbc (dsn="IDI_Clean_&version._srvprd"); 
	create table project.HESAddress as
	select snz_uid, mdy(hes_hhd_month_nbr,1,hes_hhd_year_nbr) as StartDate format yymmdd10., Region, TA, 'hes' as Source
	from connection to odbc
	(select snz_uid, hes_add_region_code as Region, hes_add_ta_code as TA,
		b.hes_hhd_month_nbr, b.hes_hhd_year_nbr
		from hes_clean.hes_address A LEFT JOIN hes_clean.hes_household B
		on a.snz_hes_hhld_uid = b.snz_hes_hhld_uid
		where A.hes_add_ta_code is not NULL
		order by snz_uid);
disconnect from odbc;
quit;

* Household Labour Force Survey - impute date at start of quarter;
proc sql;
	Connect to odbc (dsn="IDI_Clean_&version._srvprd"); 
	create table project.HLFSAddress as
	select snz_uid, intnx('month', input(hlfs_adr_quarter_date,yymmdd10.), -2) as StartDate format yymmdd10., Region, TA, 'hlfs' as Source
	from connection to odbc
	(select snz_uid, hlfs_adr_quarter_date, hlfs_adr_region_code as Region, hlfs_adr_ta_code as TA
		from hlfs_clean.household_address
		where hlfs_adr_ta_code is not NULL and hlfs_adr_quarter_date is not NULL
		order by snz_uid, hlfs_adr_quarter_date);
	disconnect from odbc;
quit;

* Arrival and departure cards;
Proc SQL;
	Connect to odbc (dsn="IDI_Clean_&version._srvprd"); 
	CREATE TABLE project.PLTAddress AS 
	SELECT snz_uid, datepart(StartDate) as StartDate format yymmdd10., TA, 'plt' as Source, Direction
	FROM connection to  odbc (
		select distinct snz_uid, cus_jou_actual_date as StartDate, cus_jou_ta_code as TA, cus_jou_direction_code as direction
		from cus_clean.journey
		where cus_jou_ta_code is not NULL and cus_jou_actual_date is not NULL
		order by snz_uid);
	DISCONNECT FROM odbc ;
Quit;

data project.PLTDAddress;
	set project.PLTAddress;
	where direction='D';
	drop direction;
run;

data project.PLTAAddress;
	set project.PLTAddress;
	where direction='A';
	drop direction;
run;

** Get a postcode to TA match from the address notification table;
** Create a postcode to TA concordance as it is not a clean 1:1 match, just take the best match;
proc freq data=data.address_notification noprint;
	tables ant_post_code*ant_ta_code/out=pc_ta;
run;
proc sort data=pc_ta;
	by ant_post_code count;
run;
data postcode_ta(drop=count percent);
	set pc_ta;
	by ant_post_code;
	if last.ant_post_code and ant_post_code ne '';
run;

** Student loans and allowances;
%macro address_Import_SLA(Out = , In = , Prefix = , Source = , indate = );
Proc SQL;
	Connect to odbc (dsn="IDI_Clean_&version._srvprd"); 
	CREATE TABLE &out. AS 
	SELECT snz_uid, input(StartDate,yymmdd10.) as StartDate format yymmdd10., 
			Region, TA, post_code, &source. as Source
	FROM connection to  odbc (
		select distinct snz_uid, &indate. as StartDate, &prefix._region_code as Region, &prefix._ta_code as TA , &prefix._post_code as post_code
		from &in.
		where (&prefix._ta_code is not NULL or &prefix._post_code is not NULL) and &indate. is not NULL
		order by snz_uid);
	DISCONNECT FROM odbc;
Quit;

proc sql;
	create table &out. as
	select a.snz_uid, a.StartDate, a.Source, a.post_code, coalesce(a.ta,b.ant_ta_code) as ta
	from &out. a left join postcode_ta b
	on a.post_code = b.ant_post_code
	order by snz_uid;
quit;

data &out.(drop=post_code) miss;
	set &out.;
	if ta ne '' then output &out.;
	else output miss;
run;
%mend address_Import_SLA;

%Address_Import_SLA(Out = project.SLAIRAddress, In = sla_clean.ird_post_code, Prefix = ir_apc, Source = 'slair', indate = ir_apc_applied_date);
%Address_Import_SLA(Out = project.SLAPAddress, In = sla_clean.msd_borrowing, Prefix = msd_sla_postal, Source = 'slap', indate = coalesce(msd_sla_sl_study_start_date,msd_sla_sa_study_start_date));
%Address_Import_SLA(Out = project.SLASAddress, In = sla_clean.msd_borrowing, Prefix = msd_sla_study, Source = 'slas', indate = coalesce(msd_sla_sl_study_start_date,msd_sla_sa_study_start_date));

* Census;
proc sql;
	Connect to odbc (dsn="IDI_Clean_&version._srvprd"); 
	create table CensusAddress as
	select snz_uid, mdy(3,5,2013) as StartDate format yymmdd10., Region, TA, 'cen' as Source
	from connection to odbc
		(select a.snz_uid, b.cen_are_ta_code as ta, b.cen_are_reg_council_code as region
		from cen_clean.census_individual a left join cen_clean.census_area b
		on a.cen_ind_usu_admin_mb_code = b.cen_are_admin_mb_code
		where a.cen_ind_indiv_rec_type_code in (3,4) 
		order by snz_uid);
	disconnect from odbc;
quit;

* Now create a Census address file that just has locations where an address is recorded;
data project.CensusAddress(drop=address_ind address_type_code) project.CensusAddress_full(drop=address_type_code);
	merge CensusAddress(in=a) cen.census_address(in=b keep=snz_uid ta_code address_type_code where=(address_type_code='UR' and ta_code ne ''));
	by snz_uid;
	if ta_code ne '' then ta=ta_code;
	if a and b then do;
		address_ind=1;
		if last.snz_uid then output project.CensusAddress;
	end;
	else if a then address_ind=0;
	else delete;
	if last.snz_uid then output project.CensusAddress_full;
run;

* Census 5 years ago;
proc sql;
	create table au_ta as
	select distinct au_code as au, au_name_text as au_name, ta_code as ta, ta_name_text as ta_name
	from metadata.meshblock_current;
quit;

proc sql;
	create table project.CensusAddress5 as
	select a.snz_uid, mdy(3,5,2008) as StartDate format yymmdd10., TA, 'cen' as Source, a.cen_ind_addr_5yrs_ago_au_code as au5,
		a.cen_ind_addr_5yrs_ago_code as au5_code
	from cen.census_individual a left join au_ta b
	on a.cen_ind_addr_5yrs_ago_au_code = b.au
	order by snz_uid;
quit;

proc freq data=project.CensusAddress5;
	tables ta;
run;

data project.CensusAddress5;
	set project.CensusAddress5;
	if ta='' and '999001' le au5 le '999076' then ta=substr(au5,4,3);
	else if ta in ('','999') then delete;
run;

proc freq data=project.CensusAddress5;
	tables ta;
run;

** Select start dates and end dates for all PAYE employment records from June 2002 onwards;
proc sql;
	Connect to odbc (dsn="IDI_Clean_&version._srvprd"); 
	create table employee_pbn as
	select *
	from connection to odbc
		(select snz_uid, inc_pbn_pbn_nbr as pbn, inc_pbn_dim_month_key as month
		from data.income_pbn_ent 
		where inc_pbn_pbn_nbr is not NULL and inc_pbn_dim_month_key > 200206);
	disconnect from odbc;
quit;

proc sort data=employee_pbn;
	by snz_uid pbn month;
run;

data employee_pbn2(drop=month lagmonth);
	set employee_pbn;
	retain firstmonth lagmonth;
	by snz_uid pbn;
	if first.pbn then do;
		firstmonth=month;
		lagmonth=.;
	end;
	if month-lagmonth not in (1,89) and lagmonth ne . then do;
		lastmonth=lagmonth;
		output;
		firstmonth=month;
		lagmonth=.;
	end;
	if first.pbn and last.pbn then do;
		lastmonth=firstmonth;
		output;
	end;
	else if last.pbn then do;
		lastmonth=month;
		output;
	end;
	else lagmonth=month;
	delete;
run;

** Add on the TA location for each spell with each PBN;
proc sql;
	create table project.EMPAddress as
	select a.*, b.br_pbn_geo_ta_code as ta
	from employee_pbn2 a left join br.pbn b
	on a.pbn=b.br_pbn_pbn_nbr and b.br_pbn_dim_start_month_key <= a.firstmonth <= b.br_pbn_dim_end_month_key 
	where b.br_pbn_geo_ta_code ne ''
	order by snz_uid, firstmonth;
quit;

** Now we need to de-dupe the employment TAs 
** - if a spell sits completely within another spell, remove the spell that sits within it
** - if a spell overlaps a subsequent spell, stop the first spell when the new spell starts;

data project.EMPAddress(drop=lagfirst laglast pbn);
	retain lagfirst laglast;
	set project.EMPAddress;
	by snz_uid;
	if first.snz_uid then do;
		lagfirst=.;
		laglast=.;
	end;
	if lastmonth<=laglast then delete;
	else if firstmonth<=laglast then firstmonth=laglast+1;
	if substr(put(firstmonth,6.),5,2)=13 then firstmonth=laglast+89;
	output;
	lagfirst=firstmonth;
	laglast=lastmonth;
run;

data project.EMPAddress(drop=firstmonth lastmonth);
	set project.EMPAddress;
	firstdate=input(put(firstmonth,6.)||'01',yymmdd10.);
	lastdate=intnx('month',input(put(lastmonth,6.)||'01',yymmdd10.),0,'E');
	format firstdate lastdate date9.;
run;

*** Assess how many addresses we pick up from each source and their distribution by year and age - and output the results;
data source_age;
run;
%macro source_ta(source);
*ods excel options(sheet_name="Dates &source.");
%if &source.=emp %then %do; %let date=lastdate; %end;
%else %do; %let date=startdate; %end;

%if &source.=census %then %do; 
proc sql;
	create table &source.Address as
	select a.snz_uid, int((a.&date. - mdy(b.snz_birth_month_nbr,15,b.snz_birth_year_nbr))/365.25) as age
	from project.CENSUSADDRESS_FULL a left join data.personal_detail b
	on a.snz_uid = b.snz_uid;
quit;
run;
%end;

%else %do;
proc sql;
	create table &source.Address as
	select a.snz_uid, int((a.&date. - mdy(b.snz_birth_month_nbr,15,b.snz_birth_year_nbr))/365.25) as age
	from project.&source.Address a left join data.personal_detail b
	on a.snz_uid = b.snz_uid
	where '01jul2015'd <= &date. < '01jul2016'd ;
quit;
run;
%end; 

proc freq data=&source.Address;
	tables age/out=&source._age;
	where 0<=age<=100;
	title "TA ages - source &source.";
run;

data source_age;
	length source $ 7;
	retain source age count;
	set source_age &source._age(in=b);
	if age=. then delete;
	if b then source="&source.";
	drop percent;
run;
%mend source_ta;

%source_ta(source=acc);
%source_ta(source=census);
%source_ta(source=dlr);
%source_ta(source=hes);
%source_ta(source=hlfs);
%source_ta(source=hnzn);
%source_ta(source=hnzr);
%source_ta(source=hnzt);
%source_ta(source=ird);
%source_ta(source=moe);
%source_ta(source=moesch);
%source_ta(source=moeter);
%source_ta(source=msdr);
%source_ta(source=msdp);
%source_ta(source=mvr);
%source_ta(source=nhi);
%source_ta(source=pho);
%source_ta(source=plta);
%source_ta(source=pltd);
%source_ta(source=emp);
%source_ta(source=slair);
%source_ta(source=slap);
%source_ta(source=slas);

ods listing close;
ods html close;
data source_age;
	set source_age;
	if count<6 then count=0;
run;

%rr3(source_age,source_age,count);
ods excel file="&path.\Tables\Address sources by age.xlsx";
proc print data=source_age;
run;
ods excel close;
ods html;
ods listing;