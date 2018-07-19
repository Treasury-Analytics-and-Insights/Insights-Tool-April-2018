****************************************************************************
Purpose: This code produces an IDI resident population (the IDI-ERP)
 at 30 June of a given year.
                                                                     
Beginning with the IDI spine, individuals are retained if they have had 
 activity in education, health, IR tax, or ACC datasets in the previous 
 twelve months, or if they have a birth record in the previous five years.                              
                                                                     
Individuals are then removed if they have left the population by death 
 or outmigration prior to the reference date.      

Version:  This is Version 2 of the IDI-ERP, as used in the experimental    
 series released on 30 September 2016.     
                                                                     
Authors: Sheree Gibb and Emily Shrosbree (Statistics New Zealand)                        
Updated by Nathaniel Matheson-Dunning (September 2016)                   

Modified by Keith McLeod (The Treasury) August 2017 for regional migration project.
	- Incorporates Prison activity indicator.
	- Allow activity indicators to be in any of the 12 months either before or after the
		date of interest.
	- Allow being issued a valid student, work or residence visa in that year as an activity.
	- Tighten the rule to have to be at least 12 out of 16 months centred on the date of interest
		in NZ. This will exclude many working holidaymakers or short-term students. Move the 
		application of this rule to separate code that also constructs a consistent ERP across years
		of interest.

****************************************************************************;
%macro idierp_jun2(year);
%let prevyear = %eval(&year. - 1);
%let nextyear = %eval(&year. + 1);

* Create a list of all people in the spine;
data spinepop;
   set data.personal_detail (where = (snz_spine_ind = 1 and snz_person_ind = 1));
   keep snz_uid snz_spine_ind snz_sex_code snz_birth_year_nbr snz_birth_month_nbr snz_deceased_year_nbr snz_deceased_month_nbr;
run;
proc sort; by snz_uid;

**********************************************************************************************************
*** Produce a list of all individuals with activity in relevant datasets in last 12 months / 5 years   ***
**********************************************************************************************************;

*********************************************************************************
***   Identify individuals with activity in education datasets in last year   ***
*********************************************************************************;

** Tertiary enrolments;
proc sql;
   create table tertiary as 
   select distinct snz_uid, 1 as flag_ed
   from moe.course
   where (moe_crs_start_date <= "&nextyear.-06-30" and moe_crs_end_date >= "&prevyear.-07-01")
   order by snz_uid;
quit;

** Industry training;
proc sql;
   create table industry_tr as
   select distinct snz_uid, 1 as flag_ed
   from moe.tec_it_learner
   where (moe_itl_start_date <= "&nextyear.-06-30" and moe_itl_end_date >= "&prevyear.-07-01")
   order by snz_uid;
quit;

** School enrolments;
proc sql;
   create table school as 
   select distinct snz_uid, 1 as flag_ed
   from moe.student_enrol 
   where ((moe_esi_start_date <= "&nextyear.-06-30") and (moe_esi_end_date >= "&prevyear.-07-01" or moe_esi_end_date is NULL))
   order by snz_uid;
quit;

***************************************************************************
***   Identify individuals with activity in tax datasets in last year   ***
***************************************************************************;

** EMS (tax at source) dataset;
proc sql;
   create table ems as
   select distinct snz_uid, 1 as flag_ir, 1 as flag_ems
   from ird.ird_ems
   where ir_ems_return_period_date >= "&prevyear.-07-01" and ir_ems_return_period_date <= "&nextyear.-06-30"
   order by snz_uid;
quit;

** Self-employment income;
proc sql;
   create table selfemp as 
   select distinct snz_uid as snz_uid, 1 as flag_ir, 1 as flag_se
   from data.income_tax_yr_summary
   where (inc_tax_yr_sum_year_nbr in (&year.,&nextyear.))
      and (inc_tax_yr_sum_S00_tot_amt <> 0 or inc_tax_yr_sum_S01_tot_amt <> 0 or inc_tax_yr_sum_S02_tot_amt <> 0 or inc_tax_yr_sum_S03_tot_amt <> 0
        or inc_tax_yr_sum_C00_tot_amt <> 0 or inc_tax_yr_sum_C01_tot_amt <> 0 or inc_tax_yr_sum_C02_tot_amt <> 0
        or inc_tax_yr_sum_P01_tot_amt <> 0 or inc_tax_yr_sum_P02_tot_amt <> 0) 
   order by snz_uid;
quit;

**********************************************************************************
***   Identify individuals with activity in health datasets in the last year   ***
**********************************************************************************;

** GMS claims;
proc sql;
   create table gms_activity as
   select distinct snz_uid
   from moh.gms_claims
   where moh_gms_visit_date >= "&prevyear.-07-01" and moh_gms_visit_date <= "&nextyear.-06-30"
   order by snz_uid;
quit;

** Laboratory tests;
proc sql;
   create table lab_claims as
   select distinct snz_uid
   from moh.lab_claims
   where moh_lab_visit_date >= "&prevyear.-07-01" and moh_lab_visit_date <= "&nextyear.-06-30"
   order by snz_uid;
quit;

** Non-admissions events;
proc sql;
   create table nnpac as
   select distinct snz_uid
   from moh.nnpac
   where moh_nnp_service_date >= "&prevyear.-07-01" and moh_nnp_service_date <= "&nextyear.-06-30" and moh_nnp_attendence_code = 'ATT'
   order by snz_uid;
quit;

** Prescriptions dispensed;
proc sql;
   create table pharma as
   select distinct snz_uid
   from moh.pharmaceutical
   where moh_pha_dispensed_date >= "&prevyear.-07-01" and moh_pha_dispensed_date <= "&nextyear.-06-30"
   order by snz_uid;
quit;

** Consultation with PHO-registered GP;
proc sql;
   create table pho as
   select distinct snz_uid
   from moh.pho_enrolment
   where moh_pho_last_consul_date >= "&prevyear.-07-01" and moh_pho_last_consul_date <= "&nextyear.-06-30"
   order by snz_uid;
quit;

** Discharged from publically funded hospitals;
proc sql;
   create table hospital as
   select distinct snz_uid
   from moh.pub_fund_hosp_discharges_event
   where moh_evt_even_date >= "&prevyear.-07-01" and moh_evt_evst_date <= "&nextyear.-06-30"
   order by snz_uid;
quit;

** Combine all health activity datasets to get list of all people with health activity in last year;
data health_activity;
   merge gms_activity lab_claims nnpac pharma pho hospital;
   by snz_uid;
   flag_health = 1;
run;

**********************************************************************
***   Identify individuals with activity in ACC in the last year   ***
**********************************************************************;

** ACC claims (date of file within the last year, not date of accident);
proc sql;
   create table acc as
   select distinct snz_uid, 1 as flag_acc
   from acc.claims
   where acc_cla_lodgement_date >= "&prevyear.-07-01" and acc_cla_lodgement_date <= "&nextyear.-06-30"
   order by snz_uid;
quit;

******************************************
***   Get births in the last 5 years   ***
******************************************;
proc sql;
   create table births as
   select snz_uid, 1 as flag_birth
   from dia.births
   where ((dia_bir_birth_year_nbr > %eval(&year.- 5) and dia_bir_birth_year_nbr < &year.) 
      or (dia_bir_birth_year_nbr = %eval(&year. - 5) and dia_bir_birth_month_nbr > 6)
      or (dia_bir_birth_year_nbr = &year. and dia_bir_birth_month_nbr <= 6))
      and dia_bir_still_birth_code IS NULL
   order by snz_uid;
quit;

***************************************************
***   Get visa approvals for children under 5   ***
***************************************************;
proc sql;
   create table visas_under5 as 
   select distinct snz_uid, 1 as flag_visa_under5
   from dol.decisions
   where ((dol_dec_birth_year_nbr > %eval(&year.- 5)) 
      or (dol_dec_birth_year_nbr = %eval(&year.- 5) and dol_dec_birth_month_nbr > 6))
      and dol_dec_decision_date < "&year.-07-01"
      and dol_dec_decision_type_code = 'A' 
      and dol_dec_application_type_code not in ('20', '21') /*Excludes visitor visas and transit visas*/
   order by snz_uid;
quit;

***************************************************
***   Get visa approvals for adults - only visas approved during the two year period   ***
***************************************************;
proc sql;
   create table visas_adult as 
   select distinct snz_uid, 1 as flag_visa
   from dol.decisions
   where ((dol_dec_birth_year_nbr <= %eval(&year.- 5)) 
      or (dol_dec_birth_year_nbr = %eval(&year.- 5) and dol_dec_birth_month_nbr <= 6))
      and dol_dec_decision_date >= "&prevyear.-07-01" and dol_dec_decision_date < "&nextyear.-07-01"
      and dol_dec_decision_type_code = 'A' 
      and dol_dec_application_type_code not in ('20', '21') /*Excludes visitor visas and transit visas*/
   order by snz_uid;
quit;


/*****************************************************************************************
	Activity - prison
******************************************************************************************/

*Check if a person has spent any time in prison or remand in the year;
PROC SQL;
	CREATE TABLE prison AS
	SELECT DISTINCT 
		snz_uid, 1 as flag_prison
	FROM cor.ov_major_mgmt_periods
	WHERE cor_mmp_mmc_code IN ('PRISON', 'REMAND' , 'HD_REL', 'HD_SENT')
 		and cor_mmp_period_end_date >= "&prevyear.-07-01" and cor_mmp_period_start_date <= "&nextyear.-06-30"
	ORDER BY snz_uid;
QUIT;


******************************************************************************
***   Combine all activity files with spine to create a total population   ***
******************************************************************************;

** Combine all activity sources **;
data total_activity_pop;
   merge selfemp ems industry_tr tertiary school health_activity acc births visas_under5 visas_adult prison;
   by snz_uid;
   if flag_ed = . then flag_ed = 0;
   if flag_ir = . then flag_ir = 0;
   if flag_health = . then flag_health = 0;
   if flag_acc = . then flag_acc = 0;
   if flag_se = . then flag_se = 0;
   if flag_ems = . then flag_ems = 0;
   if flag_prison = . then flag_prison = 0;
   if flag_visa = . then flag_visa=0;
   if flag_birth = 1 or flag_visa_under5 = 1 then flag_under5 = 1;
   else flag_under5 = 0;
   activity_flag = 1;
run;

** Combine all individuals who have activity and are in the spine **;
data idierp_Jun&year.;
   merge total_activity_pop (in=a) spinepop (in=b);
   by snz_uid;

   ** Only include invididuals who had activity and are in spine;
   if a and b;

   ** Calculate age from birth year and month **;
   age = &year. - snz_birth_year_nbr;
   if snz_birth_month_nbr > 6 then age = age - 1;

   ** Remove people with no date of birth or sex information;
   if age < 0 then delete;
   if snz_sex_code = '' then delete;
   ** Remove individuals with deaths prior to reference date ;
   if (snz_deceased_year_nbr < &year. or (snz_deceased_year_nbr = &year. and snz_deceased_month_nbr <= 6))
         and snz_deceased_year_nbr ne . then delete;

   rename snz_sex_code = sex;
   keep snz_uid snz_sex_code age flag_ir flag_ed flag_health flag_acc flag_visa flag_under5 flag_prison snz_birth_year_nbr snz_birth_month_nbr snz_deceased_year_nbr snz_deceased_month_nbr;
run;
%mend idierp_jun2;