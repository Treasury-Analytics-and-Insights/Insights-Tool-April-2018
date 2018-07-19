** This code runs all of the code to construct TA level population change estimates;
** Data is primarily for use in the Insights online tool;
** Written by: Keith McLeod, Analytics and Insights, The Treasury;
** 
** Last modified by: Keith McLeod
** Date last modified: 8 Jan 2018;

* Set IDI version;
%let version=archive;

* Set file location where code and data is to be stored - this should have 'Data', 'Decision rules', and 'Tables' subfolders;
%let path=\\wprdsas10\treasurydata\MAA2013-16 Citizen pathways through human services\Keith\Regional migration;

* Run the code to assign libnames and formats;
%include "&path.\Assign libnames and run formats.sas";

** Firstly we want to construct the IDIERP as at June each year from 2008 to 2016;
** This has been modified to not yet have a time in NZ requirement - so at the moment only uses activity indicators to construct the population;
%include "&path.\Construct IDIERP (v2) at 30 June.sas";
%idierp_jun2(2008);
%idierp_jun2(2009);
%idierp_jun2(2010);
%idierp_jun2(2011);
%idierp_jun2(2012);
%idierp_jun2(2013);
%idierp_jun2(2014);
%idierp_jun2(2015);
%idierp_jun2(2016);

** Now create a list of all people who had activity in any of the periods of interest - we will use this to extend the ERP
** If someone meets the activity rule in one year they are considered as meeting it in all years;
data project.idierp_2008_2016(drop=flag: age);
	merge idierp_jun2008 idierp_jun2009 idierp_jun2010 idierp_jun2011 idierp_jun2012 idierp_jun2013 idierp_jun2014 idierp_jun2015 idierp_jun2016;
	by snz_uid;
run;

** Now apply the time in NZ rule - at least 12 months between November of the previous year and February of the next year i.e. a 16 month period centred on June;
%include "&path.\Exclude overseas from June IDIERP.sas";
%idierp_jun2b(2008);
%idierp_jun2b(2009);
%idierp_jun2b(2010);
%idierp_jun2b(2011);
%idierp_jun2b(2012);
%idierp_jun2b(2013);
%idierp_jun2b(2014);
%idierp_jun2b(2015);
%idierp_jun2b(2016);

** Construct the various location datasets at TA level - import the data and format the various datasets;
%include "&path.\Create TA datasets.sas";

** And create rules to associate TA to a person at a particular date - use 2013 Census to derive the rules and assess the quality of the match;
%include "&path.\Associate TAs with Census data and assess quality.sas";

** Cross-validate the TA assignment rules against 5-years-ago 2013 Census location (i.e. 2008), HLFS, HES;
%include "&path.\Assess quality of match to 2008 Census location and HLFS HES location.sas";

*** Now Assign TAs to the IDI ERP at each year using rules developed from the 2013 Census - define and describe population change over time, and output the data;
%include "&path.\Assign TAs to IDIERP 2.sas";