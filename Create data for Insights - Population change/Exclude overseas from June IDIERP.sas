** Apply the time in NZ rule - at least 12 months between November of the previous year and February of the next year i.e. a 16 month period centred on June;
** Also extend the ERP to include anyone with activity in ANY year of interest;
%macro idierp_jun2b(year);
%let prevyear = %eval(&year. - 1);
%let nextyear = %eval(&year. + 1);

******************************************************************************
***   Remove individuals from the population if they are living overseas   ***
******************************************************************************;
** First expand the population by including anyone who was in any of the other years ERPs;
data idierp_Jun&year.b;
	merge idierp_Jun&year.(in=a) project.idierp_2008_2016(in=b);
	by snz_uid;
	if b and not a then do;
	  flag_othyr=1;
 	  age = &year. - snz_birth_year_nbr;
	  if snz_birth_month_nbr > 6 then age = age - 1;
	  if age < 0 then delete;
	  if (snz_deceased_year_nbr < &year. or (snz_deceased_year_nbr = &year. and snz_deceased_month_nbr <= 6))
         and snz_deceased_year_nbr ne . then delete;
	end;
	else flag_othyr=0;
run;

** Calculate amount of time spent overseas in 16 months centred on the period of interest (ignore 29 Feb to make years consistent);
proc sql;
   create table overseas_spells_1yr as
   select distinct snz_uid , pos_applied_date, pos_ceased_date, pos_day_span_nbr
   from data.person_overseas_spell
   where pos_applied_date <= "28FEB&nextyear.:23:59:59.999"dt and pos_ceased_date >= "01NOV&prevyear.:00:00:00.000"dt
   order by snz_uid, pos_applied_date;
quit;

** Calculate number of days spent overseas **;
data overseas_time_1yr;
   set overseas_spells_1yr;
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
   create table time_overseas_1yr as 
   select snz_uid, ROUND(SUM(time_to_add),1) as days_overseas_last1
   from overseas_time_1yr
   group by snz_uid;
quit;

** Combine total population with time spent overseas;
data project.idierp_Jun&year.;
   merge idierp_Jun&year.b (in=a) time_overseas_1yr;
   by snz_uid;
   if a;

   ** remove people who are overseas for more than 4 months (or 122 days) out of the 16 month period spanning our date of interest;
   if days_overseas_last1 > 122 then delete;
run;
%mend idierp_jun2b;