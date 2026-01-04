# utl-altair-slc-meta-analysis-of-deaths-in-twenty-studies-with-forest-plot
Altair slc meta analysis of deaths in twenty studies with forest plot
    %let pgm=utl-altair-slc-meta-analysis-of-deaths-in-twenty-studies-with-forest-plot;

    %stop_submission;

    Altair slc meta analysis of deaths in twenty studies with forest plot

    Forest plot
    https://github.com/rogerjdeangelis/utl-altair-slc-meta-analysis-of-deaths-in-twenty-studies-with-forest-plot/blob/main/forest_random.pdf

    Too long to post here, see githib
    https://github.com/rogerjdeangelis/utl-altair-slc-meta-analysis-of-deaths-in-twenty-studies-with-forest-plot

    Problem :
       Calculate Probablility of Death based on 23 studies in four countries(random effects)
       For fixed effects change random  = TRUE to random  = FALSE

       Best estimate 0.8% death rate with a 95% CI of {0.4%,1.6%]

    NOTE: Libanme  WORKX is assigned in the autoexec file,
      so it persists from one Ultraedit submit to another Ultraedit submit

      libname workx sas7bdat "d:/wpswrkx";

    /*                     _                            _           _
     _ __   _ __ ___   ___| |_ __ _    __ _ _ __   __ _| |_   _ ___(_)___
    | `__| | `_ ` _ \ / _ \ __/ _` |  / _` | `_ \ / _` | | | | / __| / __|
    | |    | | | | | |  __/ || (_| | | (_| | | | | (_| | | |_| \__ \ \__ \
    |_|    |_| |_| |_|\___|\__\__,_|  \__,_|_| |_|\__,_|_|\__, |___/_|___/
     _                   _                                |___/
    (_)_ __  _ __  _   _| |_
    | | '_ \| '_ \| | | | __|
    | | | | | |_) | |_| | |_
    |_|_| |_| .__/ \__,_|\__|
            |_|
    */

    proc datasets lib=workx kill nolist nodetails;
    run;quit;

    options validvarname=upcase;

    data workx.have;
      retain country study total deaths;
      informat study $14. country $15.;
      input Study total deaths country;
    cards4;
    Akenroye 3921 47 US
    Batra 4545 51 Australia
    Berry2013 2939 0 US
    Berry2017 1699 50 US
    Blackburn 1055 53 US
    Cecil 4996 44 Europe
    Dosa 1430 0 US
    Edelson 4315 46 US
    Feinstein 4074 48 US
    Phelan 727 47 US
    Hudgins 1740 46 US
    Kuo 2626 49 US
    Massin 1048 0 Europe
    Noori 2979 48 Australia
    Marsh 1424 64 US
    Montalbano 4849 50 US
    Neuman 1608 47 US
    Peltz 4659 47 US
    Seo 2778 42 Korea
    Spaite 800 0 US
    Wijlaars2015 4573 0 Europe
    Wijlaars2018 4941 57 Europe
    Zook 4281 47 US US
    ;;;;
    run;quit;

    proc print data=workx.have width=min;
    run;quit;

    /*---
    Altair SLC

    Obs     COUNTRY        STUDY        TOTAL    DEATHS

      1    US           Akenroye         3921      47
      2    Australia    Batra            4545      51
      3    US           Berry2013        2939       0
      4    US           Berry2017        1699      50
      5    US           Blackburn        1055      53
      6    Europe       Cecil            4996      44
      7    US           Dosa             1430       0
      8    US           Edelson          4315      46
      9    US           Feinstein        4074      48
     10    US           Phelan            727      47
     11    US           Hudgins          1740      46
     12    US           Kuo              2626      49
     13    Europe       Massin           1048       0
     14    Australia    Noori            2979      48
     15    US           Marsh            1424      64
     16    US           Montalbano       4849      50
     17    US           Neuman           1608      47
     18    US           Peltz            4659      47
     19    Korea        Seo              2778      42
     20    US           Spaite            800       0
     21    Europe       Wijlaars2015     4573       0
     22    Europe       Wijlaars2018     4941      57
     23    US           Zook             4281      47

    ---*/

    /*
     _ __  _ __ ___   ___ ___  ___ ___
    | `_ \| `__/ _ \ / __/ _ \/ __/ __|
    | |_) | | | (_) | (_|  __/\__ \__ \
    | .__/|_|  \___/ \___\___||___/___/
    |_|
    */

    %utlfkil(d:/pdf/forest_random.pdf);

    options set=RHOME "C:\Progra~1\R\R-4.5.2\bin\r";

    proc r;
    export data=workx.have r=have;
    submit;
    library(meta)
    library(metafor)
    have;
    studies <- have$STUDY
    obs     <- have$DEATHS
    denom   <- have$TOTAL
    setting <- have$COUNTRY

    m3 <- metaprop(
      event   = obs,
      n       = denom,
      studlab = studies,
      method  = "GLMM",
      random  = TRUE
    )

    pdf("d:/pdf/forest_random.pdf",width = 8, height =11)
    forest(m3
      ,print.tau2  = FALSE
      ,text.common = "Total Fixed"
      ,rightcols   = c("effect","ci")
      ,digits      = 3L
      ,leftlabs    = c("Study","Deaths","Total")
      )

    # Existing dataframe
    forest_df <- data.frame(
      Study          = m3$studlab,
      Deaths         = m3$event,
      Total          = m3$n,
      effect_log_odds = m3$TE,
      lower_log_odds  = m3$lower,
      upper_log_odds  = m3$upper,
      observed_risk   = m3$event / m3$n,
      estimated_risk  = plogis(m3$TE),
      lower_est_risk  = plogis(m3$lower),
      upper_est_risk  = plogis(m3$upper)
    )

    # Create a total row using m3 (random-effects summary)
    total_row <- data.frame(
      Study          = "Total (Random effects)",
      Deaths         = sum(m3$event, na.rm = TRUE),
      Total          = sum(m3$n, na.rm = TRUE),
      effect_log_odds = m3$TE.random,
      lower_log_odds  = m3$lower.random,
      upper_log_odds  = m3$upper.random,
      observed_risk   = sum(m3$event, na.rm = TRUE) / sum(m3$n, na.rm = TRUE),
      estimated_risk  = plogis(m3$TE.random),
      lower_est_risk  = plogis(m3$lower.random),
      upper_est_risk  = plogis(m3$upper.random)
    )

    # Combine
    forest_df <- rbind(forest_df, total_row)

    forest_df
    # str(m2)
    pdf()

    endsubmit;
    import data=workx.risk r=forest_df;
    run;quit;

    proc report data=workx.risk split='_' headskip;
    format effect_logodds lower_logodds upperlogodds observed_risk estimated_risk lower_est_risk upper_est_risk 9.5;
    cols _all_;
    run;quit;

    /*           _               _
      ___  _   _| |_ _ __  _   _| |_
     / _ \| | | | __| `_ \| | | | __|
    | (_) | |_| | |_| |_) | |_| | |_
     \___/ \__,_|\__| .__/ \__,_|\__|
                    |_|
    */

    Altair SLC
                                           EFFECT      LOWER      UPPER                            LOWER      UPPER
                                              LOG        LOG        LOG   OBSERVED  ESTIMATED        EST        EST
      STUDY          DEATHS      TOTAL       ODDS       ODDS       ODDS       RISK       RISK       RISK       RISK

      Akenroye           47       3921  -4.411895  -4.721836   -4.12488    0.01199    0.01199    0.00882    0.01591
      Batra              51       4545  -4.478673  -4.775183  -4.203195    0.01122    0.01122    0.00837    0.01473
      Berry2013           0       2939  -8.679142          .  -6.679874    0.00000    0.00017    0.00000    0.00125
      Berry2017          50       1699  -3.495901  -3.798191  -3.214705    0.02943    0.02943    0.02192    0.03862
      Blackburn          53       1055  -2.939461  -3.235427  -2.662887    0.05024    0.05024    0.03785    0.06520
      Cecil              44       4996  -4.723357  -5.044047   -4.42734    0.00881    0.00881    0.00641    0.01181
      Dosa                0       1430  -7.958926          .  -5.958817    0.00000    0.00035    0.00000    0.00258
      Edelson            46       4315  -4.530493  -4.843853  -4.240622    0.01066    0.01066    0.00782    0.01419
      Feinstein          48       4074  -4.429328  -4.735744  -4.145333    0.01178    0.01178    0.00870    0.01559
      Phelan             47        727  -2.671945  -2.989885   -2.37569    0.06465    0.06465    0.04788    0.08505
      Hudgins            46       1740  -3.606206  -3.921915  -3.313622    0.02644    0.02644    0.01942    0.03511
      Kuo                49       2626  -3.962561  -4.266584  -3.680325    0.01866    0.01866    0.01384    0.02459
      Massin              0       1048  -7.648263          .  -5.647556    0.00000    0.00048    0.00000    0.00351
      Noori              48       2979  -4.111898  -4.418942  -3.827181    0.01611    0.01611    0.01190    0.02131
      Marsh              64       1424  -3.056357  -3.323272  -2.805409    0.04494    0.04494    0.03478    0.05703
      Montalbano         50       4849   -4.56414  -4.863694  -4.286085    0.01031    0.01031    0.00766    0.01357
      Neuman             47       1608  -3.502934  -3.815423  -3.212981    0.02923    0.02923    0.02155    0.03868
      Peltz              47       4659  -4.586269  -4.895933  -4.299573    0.01009    0.01009    0.00742    0.01339
      Seo                42       2778  -4.176583  -4.506416  -3.872522    0.01512    0.01512    0.01092    0.02038
      Spaite              0        800  -7.378384          .  -5.376983    0.00000    0.00062    0.00000    0.00460
      Wijlaars2015        0       4573  -9.121181          .  -7.122199    0.00000    0.00011    0.00000    0.00081
      Wijlaars2018       57       4941  -4.450669  -4.730059  -4.189962    0.01154    0.01154    0.00875    0.01492
      Zook               47       4281  -4.500755  -4.810549  -4.213909    0.01098    0.01098    0.00808    0.01457
      Total (Random)    883      68007   -4.86836  -5.638771  -4.097949    0.01298    0.00763    0.00354    0.01634

    /*              _
      ___ _ __   __| |
     / _ \ `_ \ / _` |
    |  __/ | | | (_| |
     \___|_| |_|\__,_|

    */
