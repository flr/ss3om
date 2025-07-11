#V3.30.15.00-trans;_2020_03_26;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_12.0
#Stock Synthesis (SS) is a work of the U.S. Government and is not subject to copyright protection in the United States.
#Foreign copyrights may apply. See copyright.txt for more information.
#_user_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_user_info_available_at:https://vlab.ncep.noaa.gov/group/stock-synthesis
swo.dat
swo.ctl
0 # 0=use init values in control file; 1=use ss.par
1 # run display detail (0,1,2)
1 # detailed output (0=minimal for data-limited, 1=high (w/ wtatage.ss_new), 2=brief) 
0 # write 1st iteration details to echoinput.sso file (0,1) 
0 # write parm values to ParmTrace.sso (0=no,1=good,active; 2=good,all; 3=every_iter,all_parms; 4=every,active)
1 # write to cumreport.sso (0=no,1=like&timeseries; 2=add survey fits)
0 # Include prior_like for non-estimated parameters (0,1) 
1 # Use Soft Boundaries to aid convergence (0,1) (recommended)
1 # Number of datafiles to produce: 1st is input, 2nd is estimates, 3rd and higher are bootstrap
10 # Turn off estimation for parameters entering after this phase
10 # MCeval burn interval
2 # MCeval thin interval
0 # jitter initial parm value by this fraction
1948 # min yr for sdreport outputs (-1 for styr)
2028 # max yr for sdreport outputs (-1 for endyr+1; -2 for endyr+Nforecastyrs
0 # N individual STD years 
#vector of year values 

0.0001 # final convergence criteria (e.g. 1.0e-04) 
0 # retrospective year relative to end year (e.g. -4)
1 # min age for calc of summary biomass
2 # Depletion basis:  denom is: 0=skip; 1=rel X*SPB0; 2=rel SPBmsy; 3=rel X*SPB_styr; 4=rel X*SPB_endyr
1 # Fraction (X) for Depletion denominator (e.g. 0.4)
2 # SPR_report_basis:  0=skip; 1=(1-SPR)/(1-SPR_tgt); 2=(1-SPR)/(1-SPR_MSY); 3=(1-SPR)/(1-SPR_Btarget); 4=rawSPR
1 # Annual_F_units: 0=skip; 1=exploitation(Bio); 2=exploitation(Num); 3=sum(Apical_F's); 4=true F for range of ages; 5=unweighted avg. F for range of ages
#COND 10 15 #_min and max age over which average F will be calculated with F_reporting=4 or 5
2 # F_std_basis: 0=raw_annual_F; 1=F/Fspr; 2=F/Fmsy ; 3=F/Fbtgt; where F means annual_F
0 # MCMC output detail: integer part (0=default; 1=adds obj func components); and decimal part (added to SR_LN(R0) on first call to mcmc)
0 # ALK tolerance (example 0.0001)
-1 # random number seed for bootstrap data (-1 to use long(time) as seed): # 1592288521
3.30 # check value for end of file and for version control
