** Run standard macro and library assignment code;
%include "&path.\STAND_macro_new.sas";
%include "&path.\Std_libs.sas";

** Assign other libnames;
libname project "&path.\Data";
libname nzta ODBC dsn=idi_clean_&VERSION._srvprd schema=nzta_clean;
libname yst ODBC dsn=idi_clean_&VERSION._srvprd schema=yst_clean;
libname hes ODBC dsn=idi_clean_&VERSION._srvprd schema=hes_clean;

proc format;
value yn
0 = 'No'
1 = 'Yes';

value $ ta
'000'='Not in popn (not born yet/died)'
'001'='Far North District'
'002'='Whangarei District'
'003'='Kaipara District'
'011'='Thames-Coromandel District'
'012'='Hauraki District'
'013'='Waikato District'
'015'='Matamata-Piako District'
'016'='Hamilton City'
'017'='Waipa District'
'018'='Otorohanga District'
'019'='South Waikato District'
'020'='Waitomo District'
'021'='Taupo District'
'022'='Western Bay of Plenty District'
'023'='Tauranga City'
'024'='Rotorua District'
'025'='Whakatane District'
'026'='Kawerau District'
'027'='Opotiki District'
'028'='Gisborne District'
'029'='Wairoa District'
'030'='Hastings District'
'031'='Napier City'
'032'='Central Hawkes Bay District'
'033'='New Plymouth District'
'034'='Stratford District'
'035'='South Taranaki District'
'036'='Ruapehu District'
'037'='Whanganui District'
'038'='Rangitikei District'
'039'='Manawatu District'
'040'='Palmerston North City'
'041'='Tararua District'
'042'='Horowhenua District'
'043'='Kapiti Coast District'
'044'='Porirua City'
'045'='Upper Hutt City'
'046'='Lower Hutt City'
'047'='Wellington City'
'048'='Masterton District'
'049'='Carterton District'
'050'='South Wairarapa District'
'051'='Tasman District'
'052'='Nelson City'
'053'='Marlborough District'
'054'='Kaikoura District'
'055'='Buller District'
'056'='Grey District'
'057'='Westland District'
'058'='Hurunui District'
'059'='Waimakariri District'
'060'='Christchurch City'
'062'='Selwyn District'
'063'='Ashburton District'
'064'='Timaru District'
'065'='Mackenzie District'
'066'='Waimate District'
'067'='Chatham Islands Territory'
'068'='Waitaki District'
'069'='Central Otago District'
'070'='Queenstown-Lakes District'
'071'='Dunedin City'
'072'='Clutha District'
'073'='Southland District'
'074'='Gore District'
'075'='Invercargill City'
'076'='Auckland'
'099'='Unknown'
'999'='Total NZ';

value fwd_cat
1='Same TA'
2='Out - Moved TA'
3='Out - Left NZ'
4='Out - Died'
5='Out - Area now unknown'
9='Out - Unknown location at start';

value bak_cat
1='Same TA'
2='In - Moved TA'
3='In - Arrived in NZ'
4='In - Born'
5='In - Area was unknown'
9='In - Unknown location at end';

value age_gp
0-4   = '00_04'
5-9   = '05_09'
10-14 = '10_14'
15-19 = '15_19'
20-24 = '20_24'
25-29 = '25_29'
30-34 = '30_34'
35-39 = '35_39'
40-44 = '40_44'
45-49 = '45_49'
50-54 = '50_54'
55-59 = '55_59'
60-64 = '60_64'
65-69 = '65_69'
70-74 = '70_74'
75-79 = '75_79'
80-high = '80_';

value age_gp_broad
0-14   = '00_14'
15-24 = '15_24'
25-44 = '25_44'
45-64 = '45_64'
65-high = '65_';
run;
