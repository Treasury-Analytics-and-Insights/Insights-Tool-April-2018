

libname cen ODBC dsn=idi_clean_&VERSION._srvprd schema=cen_clean;
libname cenpit ODBC dsn=idi_sandpit_srvprd schema=clean_read_cen;

libname dol ODBC dsn=idi_clean_&VERSION._srvprd schema=dol_clean;
libname hlfs ODBC dsn=idi_clean_&VERSION._srvprd schema=hlfs_clean;
libname leed ODBC dsn=idi_clean_&VERSION._srvprd schema=from_leed_clean;
libname moe ODBC dsn=idi_clean_&VERSION._srvprd schema=moe_clean;
libname msd_leed ODBC dsn=idi_clean_&VERSION._srvprd schema=from_leed_clean;
libname msd ODBC dsn=idi_clean_&VERSION._srvprd schema=msd_clean;
libname sla ODBC dsn=idi_clean_&VERSION._srvprd schema=sla_clean;
libname moe ODBC dsn=idi_clean_&VERSION._srvprd schema=moe_clean;
libname cor ODBC dsn=idi_clean_&VERSION._srvprd schema=cor_clean;
libname moj ODBC dsn=idi_clean_&VERSION._srvprd schema=moj_clean;
libname acc ODBC dsn=idi_clean_&VERSION._srvprd schema=acc_clean;
libname cus ODBC dsn=idi_clean_&VERSION._srvprd schema=cus_clean;
libname lisnz ODBC dsn=idi_clean_&VERSION._srvprd schema=lisnz_clean;
libname ms ODBC dsn=idi_clean_&VERSION._srvprd schema=ms_clean;
libname sofie ODBC dsn=idi_clean_&VERSION._srvprd schema=sofie_clean;
libname dbh ODBC dsn=idi_clean_&VERSION._srvprd schema=dbh_clean;
libname br ODBC dsn=idi_clean_&VERSION._srvprd schema=br_clean;
libname cyf ODBC dsn=idi_clean_&VERSION._srvprd schema=cyf_clean;
libname dia ODBC dsn=idi_clean_&VERSION._srvprd schema=dia_clean;
libname pol ODBC dsn=idi_clean_&VERSION._srvprd schema=pol_clean;
libname moh ODBC dsn=idi_clean_&VERSION._srvprd schema=moh_clean;
libname data ODBC dsn=idi_clean_&VERSION._srvprd schema=data;
libname wff ODBC dsn=idi_clean_&VERSION._srvprd schema=wff_clean;
libname ird ODBC dsn=idi_clean_&VERSION._srvprd schema=ir_clean;
libname hnz ODBC dsn=idi_clean_&VERSION._srvprd schema=hnz_clean;

libname sanddol ODBC dsn=idi_sandpit_srvprd schema="clean_read_DOL";
libname sandmoe ODBC dsn=idi_sandpit_srvprd schema="clean_read_moe";
libname sandir ODBC dsn=idi_sandpit_srvprd schema="clean_read_IR";
libname sandhnz ODBC dsn=idi_sandpit_srvprd schema="clean_read_HNZ";

libname sandmoh ODBC dsn=idi_sandpit_srvprd schema="clean_read_MOH_Health_Tracker";
libname sandmoh2 ODBC dsn=idi_sandpit_srvprd schema="clean_read_MOH_PRIMHD";
libname sandmoh3 ODBC dsn=idi_sandpit_srvprd schema="clean_read_MOH_NIR";
libname sandmoh4 ODBC dsn=idi_sandpit_srvprd schema="clean_read_MOH_B4SC";
libname sandmoh5 ODBC dsn=idi_sandpit_srvprd schema="clean_read_MOH_PHARMACEUTICAL";
libname sandmoj ODBC dsn=idi_sandpit_srvprd schema="clean_read_MOJ";
libname sanddia ODBC dsn=idi_sandpit_srvprd schema="clean_read_DIA";

libname sandcyf ODBC dsn=idi_sandpit_srvprd schema="clean_read_CYF";

libname sandwff ODBC dsn=idi_sandpit_srvprd schema="clean_read_WFF";

libname sandmaa ODBC dsn=idi_sandpit_srvprd schema="DL_MAA2013_16";
libname metadata ODBC dsn=idi_metadata_srvprd schema=clean_read_CLASSIFICATIONS;
libname cap ODBC dsn=idi_clean_&VERSION._srvprd schema=cap_clean ;
libname security ODBC dsn=idi_clean_&VERSION._srvprd schema=security ;