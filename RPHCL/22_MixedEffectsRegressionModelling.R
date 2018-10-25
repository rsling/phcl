# Chapter 22: Mixed-effects regression modeling
# Author: Roland Schäfer


# Reference for script and data:
# Roland Schäfer (2018). Abstractions and exemplars: the measure noun
#   phrase alternation in German. Cognitive Linguistics 29(4).
#
# DATA DOI: https://dx.doi.org/10.5281/zenodo.1254871


# If you execute this step by step, first change into the directory
# where the script and data are located.
# If you source it, use the following command:
# source("glmm.R", chdir = T)


# This erases your whole environment. It is good practice to begin
# self-contained scripts with this to avoid reliance on a specific
# state of the environment or on side-effects
rm(list=ls(all=T))


# Seed the random number generator. If you execute this script
# strictly step by step, all numbers will be exactly as in the
# chapter.
set.seed(23846)


# Required packages.
require(lme4)         # For glmer() function (estimate GLMMs).
require(fmsb)         # For NagelkerkeR2() function.
require(MuMIn)        # For r.squaredGLMM() function.
require(merTools)     # For predictInterval().


# This loads the data, i.e. the "measure" data frame and copyright
# information.
load("22_MixedEffectsRegressionModelling_Measure.RData")


# First, let's try a GLM with the measure noun lemma as a fixed
# effect.
glm.01 <- glm(Construction~1
              +Measurelemma      # Lemma effect.
              +Badness           # Item-level effects.
              +Cardinal
              +Genitives
              +Measurecase
              , data=measure, family=binomial(link=logit))
glm.01.r2 <- NagelkerkeR2(glm.01)
print(summary(glm.01))

# OUTPUT:
#
# Call:
#   glm(formula = Construction ~ 1 + Measurelemma + Badness + Cardinal +
#         Genitives + Measurecase, family = binomial(link = logit),
#       data = measure)
#
# Deviance Residuals:
#   Min       1Q   Median       3Q      Max
# -2.2371  -0.6406  -0.3252  -0.0002   3.1909
#
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)                    -18.50259 1800.99191  -0.010  0.99180
# MeasurelemmaStapel              18.76008 1800.99204   0.010  0.99169
# MeasurelemmaStückchen           16.15565 1800.99196   0.009  0.99284
# MeasurelemmaHandvoll            16.62565 1800.99196   0.009  0.99263
# MeasurelemmaPfund               15.90301 1800.99201   0.009  0.99295
# MeasurelemmaHaufe               17.16463 1800.99192   0.010  0.99240
# MeasurelemmaUnmenge             16.89728 1800.99199   0.009  0.99251
# MeasurelemmaLiter               15.60518 1800.99191   0.009  0.99309
# MeasurelemmaStück               15.67811 1800.99191   0.009  0.99305
# MeasurelemmaKubikmeter          16.12181 1800.99192   0.009  0.99286
# MeasurelemmaTonne               15.92296 1800.99191   0.009  0.99295
# MeasurelemmaMenge               16.71923 1800.99191   0.009  0.99259
# MeasurelemmaKilogramm           15.27220 1800.99192   0.008  0.99323
# MeasurelemmaEuro                15.50919 1800.99195   0.009  0.99313
# MeasurelemmaBarrel              33.40328 3325.74304   0.010  0.99199
# MeasurelemmaBüschel             17.00718 1800.99197   0.009  0.99247
# MeasurelemmaMark                -0.52899 2485.05908   0.000  0.99983
# MeasurelemmaFass                34.54839 2877.93833   0.012  0.99042
# MeasurelemmaFlasche             16.77793 1800.99193   0.009  0.99257
# MeasurelemmaFleck               17.95611 1800.99237   0.010  0.99205
# MeasurelemmaFleckchen           18.52415 1800.99212   0.010  0.99179
# MeasurelemmaFontäne             -1.99339 4346.83041   0.000  0.99963
# MeasurelemmaFranc               -0.88178 4346.83041   0.000  0.99984
# MeasurelemmaGramm               14.44713 1800.99193   0.008  0.99360
# MeasurelemmaGallone             15.58212 1800.99230   0.009  0.99310
# MeasurelemmaGlas                16.05732 1800.99192   0.009  0.99289
# MeasurelemmaHektar              -0.01866 2076.53741   0.000  0.99999
# MeasurelemmaHäufchen            18.95688 1800.99208   0.011  0.99160
# MeasurelemmaKanne               17.07501 1800.99202   0.009  0.99244
# MeasurelemmaKilowatt            16.58379 1800.99213   0.009  0.99265
# MeasurelemmaKiste               16.08296 1800.99224   0.009  0.99287
# MeasurelemmaKlafter             -0.93773 4346.83041   0.000  0.99983
# MeasurelemmaKlumpen             17.42476 1800.99197   0.010  0.99228
# MeasurelemmaKrug                15.24714 1800.99227   0.008  0.99325
# MeasurelemmaLache               18.48454 1800.99228   0.010  0.99181
# MeasurelemmaLadung              15.20443 1800.99208   0.008  0.99326
# MeasurelemmaMeter               13.84108 1800.99220   0.008  0.99387
# MeasurelemmaMasse               33.57428 4346.83041   0.008  0.99384
# MeasurelemmaMilligramm          16.03255 1800.99207   0.009  0.99290
# MeasurelemmaMilliliter          16.00887 1800.99194   0.009  0.99291
# MeasurelemmaPortion             14.73848 1800.99198   0.008  0.99347
# MeasurelemmaPrise               15.25000 1800.99199   0.008  0.99324
# MeasurelemmaQuadratmeter        15.06513 1800.99205   0.008  0.99333
# MeasurelemmaQuadratkilometer    -1.50586 3224.61832   0.000  0.99963
# MeasurelemmaSack                15.16584 1800.99222   0.008  0.99328
# MeasurelemmaSchicht             16.85355 1800.99192   0.009  0.99253
# MeasurelemmaSchüssel            15.22995 1800.99197   0.008  0.99325
# MeasurelemmaSchwall             18.96841 1800.99196   0.011  0.99160
# MeasurelemmaStreifen            17.10040 1800.99199   0.009  0.99242
# MeasurelemmaTaler               17.94484 1800.99234   0.010  0.99205
# MeasurelemmaTasse               15.34567 1800.99193   0.009  0.99320
# MeasurelemmaTeller              15.93046 1800.99195   0.009  0.99294
# MeasurelemmaTopf                17.75997 1800.99198   0.010  0.99213
# MeasurelemmaTropfen             17.60162 1800.99192   0.010  0.99220
# MeasurelemmaÜbermass            33.79807 4346.83042   0.008  0.99380
# MeasurelemmaUnze                 0.32919 2430.55107   0.000  0.99989
# MeasurelemmaBarren               1.38204 4346.83041   0.000  0.99975
# MeasurelemmaBatzen              16.52887 1800.99233   0.009  0.99268
# MeasurelemmaBecher              15.92278 1800.99203   0.009  0.99295
# MeasurelemmaBerg                18.70507 1800.99198   0.010  0.99171
# MeasurelemmaBeutel               0.20427 3248.55095   0.000  0.99995
# MeasurelemmaBiss                17.78077 1800.99216   0.010  0.99212
# MeasurelemmaBlatt                0.06456 2440.48042   0.000  0.99998
# MeasurelemmaBlock               17.27993 1800.99247   0.010  0.99234
# MeasurelemmaBrocken             16.26154 1800.99232   0.009  0.99280
# MeasurelemmaBüchse              17.12131 1800.99247   0.010  0.99241
# MeasurelemmaBund                -0.22501 4346.83041   0.000  0.99996
# MeasurelemmaBündel              18.35238 1800.99211   0.010  0.99187
# MeasurelemmaCent                 0.44277 3314.23883   0.000  0.99989
# MeasurelemmaZentimeter          15.21799 1800.99201   0.008  0.99326
# MeasurelemmaDollar              15.11803 1800.99226   0.008  0.99330
# MeasurelemmaDose                15.69869 1800.99200   0.009  0.99305
# MeasurelemmaDosis               18.39470 1800.99229   0.010  0.99185
# MeasurelemmaEimer               16.32653 1800.99195   0.009  0.99277
# MeasurelemmaEinheit             34.71822 3225.91927   0.011  0.99141
# MeasurelemmaEsslöffel           14.58503 1800.99219   0.008  0.99354
# MeasurelemmaExtraportion        -0.12490 2608.97355   0.000  0.99996
# MeasurelemmaFeinunze            34.85118 4346.83042   0.008  0.99360
# MeasurelemmaFetzen              18.67891 1800.99225   0.010  0.99172
# MeasurelemmaFläschchen          17.82729 1800.99247   0.010  0.99210
# MeasurelemmaFranke              -0.01048 2899.09048   0.000  1.00000
# MeasurelemmaFuhre                0.59805 4346.83042   0.000  0.99989
# MeasurelemmaGläschen            15.93504 1800.99211   0.009  0.99294
# MeasurelemmaGroschen             0.59805 4346.83042   0.000  0.99989
# MeasurelemmaGulden              16.48513 1800.99232   0.009  0.99270
# MeasurelemmaHälmchen             0.27044 4346.83041   0.000  0.99995
# MeasurelemmaHappen               0.25802 3325.82979   0.000  0.99994
# MeasurelemmaHauch               16.24886 1800.99201   0.009  0.99280
# MeasurelemmaHäuflein            16.52445 1800.99215   0.009  0.99268
# MeasurelemmaHektoliter          17.15888 1800.99249   0.010  0.99240
# MeasurelemmaKanister            -0.25627 2765.94672   0.000  0.99993
# MeasurelemmaKännchen            16.79370 1800.99252   0.009  0.99256
# MeasurelemmaKante                0.55011 2897.76067   0.000  0.99985
# MeasurelemmaKarton               0.59805 4346.83042   0.000  0.99989
# MeasurelemmaKelle               -0.68692 4346.83041   0.000  0.99987
# MeasurelemmaKilowattstunde      16.87585 1800.99212   0.009  0.99252
# MeasurelemmaKlecks              18.39262 1800.99212   0.010  0.99185
# MeasurelemmaKoffer               1.03203 4346.83041   0.000  0.99981
# MeasurelemmaKorb                17.75259 1800.99247   0.010  0.99214
# MeasurelemmaKorn                 0.63159 4346.83042   0.000  0.99988
# MeasurelemmaKübel               35.73018 4346.83041   0.008  0.99344
# MeasurelemmaKugel                0.59900 2895.78949   0.000  0.99983
# MeasurelemmaLage                16.34854 1800.99207   0.009  0.99276
# MeasurelemmaLaib                19.52971 1800.99225   0.011  0.99135
# MeasurelemmaLawine              35.38017 4346.83041   0.008  0.99351
# MeasurelemmaLöffel              15.71824 1800.99202   0.009  0.99304
# MeasurelemmaMass                17.21681 1800.99248   0.010  0.99237
# MeasurelemmaMegawatt             1.08797 4346.83041   0.000  0.99980
# MeasurelemmaMegawattstunde      -0.09623 4346.83041   0.000  0.99998
# MeasurelemmaMesserspitze         0.64336 1991.58708   0.000  0.99974
# MeasurelemmaMillimeter          14.61742 1800.99220   0.008  0.99352
# MeasurelemmaMolekül             -0.88023 4346.83041   0.000  0.99984
# MeasurelemmaPäckchen             1.74319 4346.83041   0.000  0.99968
# MeasurelemmaPackung              0.18306 2212.90400   0.000  0.99993
# MeasurelemmaPaket                1.05718 3264.63474   0.000  0.99974
# MeasurelemmaPfütze              35.65183 4346.83042   0.008  0.99346
# MeasurelemmaPott                 0.49477 2895.68372   0.000  0.99986
# MeasurelemmaQuadratzentimeter   -0.20660 2908.62911   0.000  0.99994
# MeasurelemmaRaummeter            1.22227 4346.83041   0.000  0.99978
# MeasurelemmaRest                16.69200 1800.99206   0.009  0.99261
# MeasurelemmaRiegel               0.68753 4346.83042   0.000  0.99987
# MeasurelemmaRiesenportion        0.00661 3322.82244   0.000  1.00000
# MeasurelemmaRinnsal             35.00080 2638.15318   0.013  0.98941
# MeasurelemmaSchälchen            0.15855 4346.83042   0.000  0.99997
# MeasurelemmaScheffel            -0.99212 4346.83041   0.000  0.99982
# MeasurelemmaScheibchen           0.65399 4346.83042   0.000  0.99988
# MeasurelemmaScheibe             16.80685 1800.99195   0.009  0.99255
# MeasurelemmaScheit               0.03034 2864.65524   0.000  0.99999
# MeasurelemmaSchluck             16.72405 1800.99196   0.009  0.99259
# MeasurelemmaSchoppen            17.11612 1800.99246   0.010  0.99242
# MeasurelemmaSchuss              16.34122 1800.99213   0.009  0.99276
# MeasurelemmaSechserpack         -0.01966 4346.83041   0.000  1.00000
# MeasurelemmaSpritzer            35.23024 3314.23883   0.011  0.99152
# MeasurelemmaStich                0.76589 4346.83042   0.000  0.99986
# MeasurelemmaStrahl              17.60555 1800.99247   0.010  0.99220
# MeasurelemmaTafel               16.39533 1800.99209   0.009  0.99274
# MeasurelemmaTankfüllung         33.84992 4346.83041   0.008  0.99379
# MeasurelemmaTässchen            34.61307 3223.31697   0.011  0.99143
# MeasurelemmaTeelöffel            0.65981 1863.77832   0.000  0.99972
# MeasurelemmaTeilchen            35.11947 2898.76765   0.012  0.99033
# MeasurelemmaThermoskanne        -0.30853 3193.92183   0.000  0.99992
# MeasurelemmaTröpfchen           18.11780 1800.99236   0.010  0.99197
# MeasurelemmaTube                 0.68166 2505.57499   0.000  0.99978
# MeasurelemmaTupfer              35.34663 4346.83041   0.008  0.99351
# MeasurelemmaTütchen              0.21187 3326.46783   0.000  0.99995
# MeasurelemmaTüte                 0.93373 4346.83042   0.000  0.99983
# MeasurelemmaUnsumme             17.94641 1800.99235   0.010  0.99205
# MeasurelemmaViertelliter         0.64363 4346.83042   0.000  0.99988
# MeasurelemmaWatt                -0.20813 4346.83041   0.000  0.99996
# MeasurelemmaZentner             16.30295 1800.99235   0.009  0.99278
# Badness                         -0.12041    0.04606  -2.614  0.00894 **
#   CardinalNo                       1.16755    0.14870   7.852 4.11e-15 ***
#   Genitives                       -0.74687    0.04381 -17.049  < 2e-16 ***
#   MeasurecaseAcc                  -0.02241    0.09084  -0.247  0.80516
# MeasurecaseDat                   0.32364    0.12384   2.613  0.00896 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
# Null deviance: 5386.0  on 5062  degrees of freedom
# Residual deviance: 3862.8  on 4908  degrees of freedom
# AIC: 4172.8
#
# Number of Fisher Scoring iterations: 16

print(glm.01.r2)

# OUTPUT:
#
# $N
# [1] 5063
#
# $R2
# [1] 0.3967526

# Now let's try a GLMM with the measure noun lemma as a random
# effect and without tuning the optimiser.
glmm.01 <- glmer(Construction~1
                 +(1|Measurelemma)  # Lemma effect.
                 +Badness           # Item-level effects.
                 +Cardinal
                 +Genitives
                 +Measurecase
                 , data=measure, family=binomial(link=logit))
glmm.01.r2 <- r.squaredGLMM(glmm.01)
print(summary(glmm.01))

# OUTPUT:
#
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Construction ~ 1 + (1 | Measurelemma) + Badness + Cardinal +      Genitives + Measurecase
# Data: measure
#
# AIC      BIC   logLik deviance df.resid
# 4188.0   4233.7  -2087.0   4174.0     5056
#
# Scaled residuals:
#   Min      1Q  Median      3Q     Max
# -2.8683 -0.4807 -0.2606 -0.0855 10.6684
#
# Random effects:
#   Groups       Name        Variance Std.Dev.
# Measurelemma (Intercept) 1.252    1.119
# Number of obs: 5063, groups:  Measurelemma, 150
#
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)    -2.32135    0.17867 -12.992  < 2e-16 ***
#   Badness        -0.14065    0.04474  -3.144  0.00167 **
#   CardinalNo      1.35673    0.13947   9.727  < 2e-16 ***
#   Genitives      -0.73886    0.04239 -17.429  < 2e-16 ***
#   MeasurecaseAcc -0.01923    0.08821  -0.218  0.82740
# MeasurecaseDat  0.25047    0.12045   2.079  0.03758 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Correlation of Fixed Effects:
#   (Intr) Badnss CrdnlN Gentvs MsrcsA
# Badness      0.010
# CardinalNo  -0.599  0.039
# Genitives    0.071 -0.043 -0.019
# MeasurcsAcc -0.263  0.012 -0.046 -0.052
# MeasurecsDt -0.254  0.027  0.034  0.037  0.403

print(glmm.01.r2)

# OUTPUT:
#
# R2m       R2c
# 0.2004865 0.4209018

# Get conditional modes for Measurelemma.
glmm.01.ranef <- ranef(glmm.01, condVar = TRUE, drop = TRUE)$Measurelemma
print(glmm.01.ranef)

# OUTPUT:
#
# Kilometer            Stapel         Stückchen          Handvoll             Pfund             Haufe           Unmenge
# -0.626757313       1.759624697      -0.176504733       0.225988936      -0.290803718       0.774229114       0.439112942
# Liter             Stück        Kubikmeter             Tonne             Menge         Kilogramm              Euro
# -0.636312439      -0.681991861      -0.146480539      -0.320553312       0.350597203      -0.921603347      -0.667395105
# Barrel           Büschel              Mark              Fass           Flasche             Fleck         Fleckchen
# 0.605441548       0.535937386      -0.928096328       1.316123211       0.429775372       0.738834120       1.403359638
# Fontäne             Franc             Gramm           Gallone              Glas            Hektar          Häufchen
# -0.646433547      -0.353435340      -1.615065635      -0.377112829      -0.276876988      -1.234254111       1.834143263
# Kanne          Kilowatt             Kiste           Klafter           Klumpen              Krug             Lache
# 0.531545163       0.210318453      -0.095250854      -0.366776578       0.881822819      -0.580232880       1.142459384
# Ladung             Meter             Masse        Milligramm        Milliliter           Portion             Prise
# -0.798404482      -1.524116985       0.405512885      -0.067742866      -0.133838295      -1.353424852      -0.859914148
# Quadratmeter  Quadratkilometer              Sack           Schicht          Schüssel           Schwall          Streifen
# -0.826566889      -0.822640712      -0.681304648       0.495350891      -0.940552099       2.274443044       0.606459058
# Taler             Tasse            Teller              Topf           Tropfen          Übermass              Unze
# 0.847543632      -0.927648026      -0.392098040       1.151646507       1.209236411       0.463230272      -0.601308488
# Barren            Batzen            Becher              Berg            Beutel              Biss             Blatt
# -0.070614536       0.083129251      -0.305725453       1.958864843      -0.353707293       0.835532652      -0.761501554
# Block           Brocken            Büchse              Bund            Bündel              Cent        Zentimeter
# 0.355122029      -0.055002794       0.273408713      -0.283495406       1.299858344      -0.315633901      -0.816045156
# Dollar              Dose             Dosis             Eimer           Einheit         Esslöffel      Extraportion
# -0.656556402      -0.519350570       1.049083942      -0.027644355       1.055011508      -1.026022962      -0.721671328
# Feinunze            Fetzen        Fläschchen            Franke             Fuhre          Gläschen          Groschen
# 0.686369335       1.308822955       0.544319918      -0.480440800      -0.158935871      -0.247670124      -0.158935871
# Gulden          Hälmchen            Happen             Hauch          Häuflein        Hektoliter          Kanister
# 0.157057174      -0.200708754      -0.355480459      -0.078238687       0.081282715       0.298234056      -0.628775431
# Kännchen             Kante            Karton             Kelle    Kilowattstunde            Klecks            Koffer
# 0.148386046      -0.397945915      -0.158935871      -0.373659438       0.377535991       1.288493180      -0.094311353
# Korb              Korn             Kübel             Kugel              Lage              Laib            Lawine
# 0.503882986      -0.153390624       0.885875211      -0.361048825       0.018931609       1.880584161       0.814529732
# Löffel              Mass          Megawatt    Megawattstunde      Messerspitze        Millimeter           Molekül
# -0.438746541       0.338926979      -0.089191260      -0.224864181      -1.119461793      -0.931870203      -0.408434231
# Päckchen           Packung             Paket            Pfütze              Pott Quadratzentimeter         Raummeter
# -0.051059809      -0.982191657      -0.199458153       0.868281181      -0.415129439      -0.631808890      -0.078335111
# Rest            Riegel     Riesenportion           Rinnsal         Schälchen          Scheffel        Scheibchen
# 0.224832655      -0.145785901      -0.410472790       1.697019492      -0.220171843      -0.437019242      -0.151121847
# Scheibe            Scheit           Schluck          Schoppen            Schuss       Sechserpack          Spritzer
# 0.436692178      -0.539442431       0.302523631       0.297879703       0.003718559      -0.231676708       1.257188551
# Stich            Strahl             Tafel       Tankfüllung          Tässchen         Teelöffel          Teilchen
# -0.136378050       0.456676513       0.033519560       0.474785782       0.998466502      -1.849251236       1.571886302
# Thermoskanne         Tröpfchen              Tube            Tupfer           Tütchen              Tüte           Unsumme
# -0.490664219       0.824611622      -0.515089163       0.804269198      -0.309118489      -0.116406740       0.714833168
# Viertelliter              Watt           Zentner
# -0.137279739      -0.245825231       0.003896079
# attr(,"postVar")
# [1] 0.83522751 0.30689774 0.15413359 0.17805107 0.28110715 0.02469225 0.23557217 0.03467285 0.01327278 0.06510955 0.02019607
# [12] 0.00626447 0.05525352 0.14591404 0.85858620 0.18585062 0.71693496 0.69391412 0.08197725 0.63534836 0.41463828 0.95381074
# [23] 0.99869466 0.08674021 0.61398679 0.05443297 0.60460598 0.31743074 0.31073660 0.50810897 0.59906080 0.99420210 0.19198715
# [34] 0.56528061 0.54645951 0.35765148 0.36805819 0.98262947 0.39815831 0.13176777 0.18776224 0.22131436 0.31702808 0.84914743
# [45] 0.47870396 0.05271681 0.18469536 0.14055503 0.23039328 0.66203422 0.06527141 0.13374139 0.21053694 0.05878837 0.96919024
# [56] 0.82900411 1.17381315 0.69490246 0.31755003 0.19042791 0.97289950 0.52601511 0.77371184 0.79582001 0.67137379 0.78783479
# [67] 1.02683417 0.40513337 0.98276320 0.25009777 0.55195984 0.23635435 0.56485183 0.13881175 0.82805079 0.40103747 0.79213882
# [78] 0.95568563 0.48410246 0.83084536 0.88341689 1.09945479 0.44672122 1.09945479 0.70623408 1.07144121 0.95950571 0.27431939
# [89] 0.51396270 0.80054126 0.86475469 0.79981211 0.92534631 1.09945479 0.99198970 0.51517727 0.43190968 1.15159302 0.82125918
# [100] 1.10348829 0.99440050 0.94675384 0.40166017 0.43339720 0.97462996 0.28716106 0.79767245 1.15624108 1.05701576 0.62062663
# [111] 0.42382563 0.98183808 1.19357495 0.66588540 1.06234332 0.98886113 0.91646163 0.82070285 1.16637202 0.39151237 1.10914607
# [122] 0.93243312 0.60948395 1.05972545 0.97474464 1.10516024 0.13460673 0.86845430 0.17386138 0.78768223 0.49069226 1.05317033
# [133] 0.77481050 1.11635309 0.81219469 0.43190047 0.96702117 0.81904932 0.45093952 0.65610262 0.92040727 0.68042928 0.85400296
# [144] 0.97235567 0.98514611 1.13244880 0.66726092 1.11565040 1.04547219 0.70126451



# Access the variance-covariance estimates like so:
print(attributes(glmm.01.ranef)$postVar)

# OUTPUT:
#
# [1] 0.83522751 0.30689774 0.15413359 0.17805107 0.28110715 0.02469225 0.23557217 0.03467285 0.01327278 0.06510955 0.02019607
# [12] 0.00626447 0.05525352 0.14591404 0.85858620 0.18585062 0.71693496 0.69391412 0.08197725 0.63534836 0.41463828 0.95381074
# [23] 0.99869466 0.08674021 0.61398679 0.05443297 0.60460598 0.31743074 0.31073660 0.50810897 0.59906080 0.99420210 0.19198715
# [34] 0.56528061 0.54645951 0.35765148 0.36805819 0.98262947 0.39815831 0.13176777 0.18776224 0.22131436 0.31702808 0.84914743
# [45] 0.47870396 0.05271681 0.18469536 0.14055503 0.23039328 0.66203422 0.06527141 0.13374139 0.21053694 0.05878837 0.96919024
# [56] 0.82900411 1.17381315 0.69490246 0.31755003 0.19042791 0.97289950 0.52601511 0.77371184 0.79582001 0.67137379 0.78783479
# [67] 1.02683417 0.40513337 0.98276320 0.25009777 0.55195984 0.23635435 0.56485183 0.13881175 0.82805079 0.40103747 0.79213882
# [78] 0.95568563 0.48410246 0.83084536 0.88341689 1.09945479 0.44672122 1.09945479 0.70623408 1.07144121 0.95950571 0.27431939
# [89] 0.51396270 0.80054126 0.86475469 0.79981211 0.92534631 1.09945479 0.99198970 0.51517727 0.43190968 1.15159302 0.82125918
# [100] 1.10348829 0.99440050 0.94675384 0.40166017 0.43339720 0.97462996 0.28716106 0.79767245 1.15624108 1.05701576 0.62062663
# [111] 0.42382563 0.98183808 1.19357495 0.66588540 1.06234332 0.98886113 0.91646163 0.82070285 1.16637202 0.39151237 1.10914607
# [122] 0.93243312 0.60948395 1.05972545 0.97474464 1.10516024 0.13460673 0.86845430 0.17386138 0.78768223 0.49069226 1.05317033
# [133] 0.77481050 1.11635309 0.81219469 0.43190047 0.96702117 0.81904932 0.45093952 0.65610262 0.92040727 0.68042928 0.85400296
# [144] 0.97235567 0.98514611 1.13244880 0.66726092 1.11565040 1.04547219 0.70126451


# If we only want to extract the variance-covariance matrix from the fit.
print(VarCorr(glmm.01))

# OUTPUT:
#
# Groups       Name        Std.Dev.
# Measurelemma (Intercept) 1.1189

# Generate confidence intervals for random effect variance estimate.
glmm.01.varconf <- confint(glmm.01, parm="theta_", method="profile")
print(glmm.01.varconf)

# OUTPUT:
#
# 2.5 %   97.5 %
#   .sig01 0.8867409 1.413492

# (Not always) more robust alternative using bootstrap.
# TAKES A LONG TIME TO COMPUTE! Uncomment if desired.
# glmm.01.varconf.boot <- confint(glmm.01, parm="theta_", method="boot", nsim = 250)
# print(glmm.01.varconf.boot)


# Compare the GLM R2 and the GLMM R2.
cat("GLM Nagelkerke R2 =", glm.01.r2$R2)
cat("GLMM marginal R2 =",glmm.01.r2[[1]])
cat("GLMM conditional R2 =",glmm.01.r2[[2]])

# OUTPUT:
#
# GLM Nagelkerke R2 = 0.3967526
# GLMM marginal R2 = 0.2004865
# GLMM conditional R2 = 0.4209018


# Get prediction intervals for Measurelemma.
glmm.01.predict <- predictInterval(glmm.01, n.sims = 100)


# Plots the intercept from model for the number most frequent levels in data.
# NOTE! This works only with VI models, not with VS or VIVS models. But
# it can be adapted to handle such models.
ranef.plot <- function(model, effect, number = -1, intervals = 0.95, ...) {
  require(lme4)

  # Get conditional modes.
  .ranef <- ranef(model, condVar = TRUE, drop = T)[[effect]]

  # Get a subsample of conditional modes if necessary.
  if (number > 0 & length(.ranef) > number)
    .select <- sample(1:length(.ranef), number)
  else
    .select <- 1:length(.ranef)

  # Order indices of the selection by conditional modes.
  .select <- .select[order(.ranef[.select])]

  # Create full data frame.
  # Note: postVar gives you the conditional variance. Hence, we need
  # to calculate the stdev and derive a proper n% interval from it.
  .condvar <- attributes(.ranef)$postVar
  .qn <- qnorm(1-(1-intervals)/2)
  .df <- cbind(.ranef[.select],
               .ranef[.select]-.qn*sqrt(.condvar[.select]),
               .ranef[.select]+.qn*sqrt(.condvar[.select])
               )

  # Plot.
  dotchart(.df[,1], labels = rownames(.df), xlim = c(min(.df[,2]), max(.df[,3])), ...)

  # Add the prediction intervals.
  if (!is.null(intervals)) {
    for (.i in 1:nrow(.df)) lines(c(.df[.i,2], .df[.i,3]), c(.i,.i), lwd=2)
  }

  # Return the data frame with the selection.
  .df
}
opts.dotchart <- list(pch=19, col="black", cex=1, xlab="Prediction of conditional mode")

do.call(ranef.plot, c(list(glmm.01, "Measurelemma", 30, main = "Measure lemma random effects with 95% prediction intervals"), opts.dotchart))

# OUTPUT:
#
# [,1]        [,2]       [,3]
# Meter        -1.52411698 -2.71318400 -0.3350500
# Esslöffel    -1.02602296 -2.26721954  0.2151736
# Tasse        -0.92764803 -1.42838492 -0.4269111
# Quadratmeter -0.82656689 -1.93012945  0.2769957
# Extraportion -0.72167133 -2.46608202  1.0227394
# Sack         -0.68130465 -2.03737307  0.6747638
# Unze         -0.60130849 -2.38584909  1.1832321
# Scheit       -0.53944243 -2.36595042  1.2870656
# Löffel       -0.43874654 -1.48904049  0.6115474
# Glas         -0.27687699 -0.73415370  0.1803997
# Gläschen     -0.24767012 -1.55765530  1.0623151
# Schälchen    -0.22017184 -2.23781692  1.7974732
# Hälmchen     -0.20070875 -2.22947619  1.8280587
# Paket        -0.19945815 -2.21959382  1.8206775
# Stückchen    -0.17650473 -0.94598367  0.5929742
# Karton       -0.15893587 -2.21405395  1.8961822
# Korn         -0.15339062 -2.21227499  1.9054937
# Viertelliter -0.13727974 -2.20747903  1.9329195
# Kiste        -0.09525085 -1.61224374  1.4217420
# Tafel         0.03351956 -1.25455181  1.3215909
# Gulden        0.15705717 -1.49005215  1.8041665
# Menge         0.35059720  0.19546918  0.5057252
# Block         0.35512203 -1.39333723  2.1035813
# Scheibe       0.43669218 -0.28239489  1.1557792
# Tupfer        0.80426920 -1.12841398  2.7369524
# Dosis         1.04908394 -0.42395885  2.5221267
# Einheit       1.05501151 -0.72850273  2.8385257
# Tropfen       1.20923641  0.73401742  1.6844554
# Bündel        1.29985834  0.05233955  2.5473771
# Laib          1.88058416  0.59028284  3.1708855



# This estimates a GLMM with the measure noun lemma as a random
# effect.
glmm.02 <- glmer(Construction~1
                 +(1|Measurelemma)  # Lemma effect.
                 +Badness           # Item-level effects.
                 +Cardinal
                 +Genitives
                 +(1|Measurecase)
                 , data=measure, family=binomial(link=logit))
glmm.02.r2 <- r.squaredGLMM(glmm.02)
print(summary(glmm.02))

# OUTPUT
#
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Construction ~ 1 + (1 | Measurelemma) + Badness + Cardinal +      Genitives + (1 | Measurecase)
# Data: measure
#
# AIC      BIC   logLik deviance df.resid
# 4190.8   4230.0  -2089.4   4178.8     5057
#
# Scaled residuals:
#   Min      1Q  Median      3Q     Max
# -2.8733 -0.4818 -0.2618 -0.0864 10.5635
#
# Random effects:
#   Groups       Name        Variance Std.Dev.
# Measurelemma (Intercept) 1.229915 1.10902
# Measurecase  (Intercept) 0.008311 0.09116
# Number of obs: 5063, groups:  Measurelemma, 150; Measurecase, 3
#
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept) -2.25505    0.17750 -12.704   <2e-16 ***
#   Badness     -0.14194    0.04470  -3.175   0.0015 **
#   CardinalNo   1.34780    0.13920   9.682   <2e-16 ***
#   Genitives   -0.74268    0.04244 -17.500   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Correlation of Fixed Effects:
#   (Intr) Badnss CrdnlN
# Badness     0.019
# CardinalNo -0.599  0.040
# Genitives   0.074 -0.041 -0.016

print(glmm.02.r2)

# OUTPUT:
#
# R2m       R2c
# 0.1989953 0.4180434

# Generate confidence intervals for random effect variance estimate.
glmm.02.varconf <- confint(glmm.02, parm="theta_", method="profile")
print(glmm.02.varconf)

# OUTPUT:
#
# 2.5 %    97.5 %
#   .sig01 0.8775464 1.4021895
# .sig02 0.0000000 0.3903169

# Full model as used in Schäfer (2018).
# Notice that it is difficult to put the random number generator in
# exactly the same state as it was in the script used for the paper.
# This is why number differ from the journal paper. However, they are
# exactly the same as in the PHCL chapter.
glmm.03 <- glmer(Construction~1
                 +(1|Measurelemma)
                 +(1|Kindlemma)
                 +Badness
                 +Cardinal
                 +Genitives
                 +Measurecase
                 +Kindattraction
                 +Kindfreq
                 +Kindgender
                 +Measureattraction
                 +Measureclass
                 +Measurefreq
                 , data=measure, family=binomial(link=logit), na.action = na.fail,
                 control=glmerControl("bobyqa"))
glmm.03.r2 <- r.squaredGLMM(glmm.03)
print(summary(glmm.03))

# OUTPUT:
#
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Construction ~ 1 + (1 | Measurelemma) + (1 | Kindlemma) + Badness +
#   Cardinal + Genitives + Measurecase + Kindattraction + Kindfreq +
#   Kindgender + Measureattraction + Measureclass + Measurefreq
# Data: measure
# Control: glmerControl("bobyqa")
#
# AIC      BIC   logLik deviance df.resid
# 3854.1   3971.6  -1909.1   3818.1     5045
#
# Scaled residuals:
#   Min      1Q  Median      3Q     Max
# -3.4552 -0.4328 -0.2140 -0.0588  9.4044
#
# Random effects:
#   Groups       Name        Variance Std.Dev.
# Measurelemma (Intercept) 0.2040   0.4516
# Kindlemma    (Intercept) 0.3654   0.6045
# Number of obs: 5063, groups:  Measurelemma, 150; Kindlemma, 87
#
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)           -3.65264    0.24297 -15.033  < 2e-16 ***
#   Badness               -0.15607    0.04736  -3.296 0.000982 ***
#   CardinalNo             1.21725    0.14602   8.336  < 2e-16 ***
#   Genitives             -0.70955    0.04622 -15.350  < 2e-16 ***
#   MeasurecaseAcc         0.03042    0.09461   0.322 0.747782
# MeasurecaseDat         0.72094    0.13036   5.530 3.19e-08 ***
#   Kindattraction         0.23445    0.08940   2.623 0.008726 **
#   Kindfreq               0.15280    0.08425   1.814 0.069733 .
# KindgenderNeut         0.02480    0.19602   0.127 0.899334
# KindgenderFem          1.30588    0.23738   5.501 3.77e-08 ***
#   Measureattraction      0.29140    0.08953   3.255 0.001134 **
#   MeasureclassContainer  0.25751    0.27059   0.952 0.341281
# MeasureclassRest       0.42500    0.33947   1.252 0.210589
# MeasureclassAmount     0.85638    0.33458   2.560 0.010482 *
#   MeasureclassPortion    1.25430    0.25327   4.952 7.33e-07 ***
#   Measurefreq           -0.23519    0.07618  -3.088 0.002018 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

print(glmm.03.r2)

# OUTPUT:
#
# R2m       R2c
# 0.4221234 0.5073817


# We now calculate a prediction for item 99 step by step.

# Part I: Getting to know the commands
# There will be no output when this is sourced.

# First, we show how to extract certain parameters and
# conditional modes from the model. We use data point (or
# item) 99 as an example. This gives you the values measured
# for thisdata point:
measure[99,]

# OUTPUT:
#
# Measurecase Kindlemma Kindgender Kindcase Kindfreq Measurelemma Measureclass Measurefreq Genitives    Badness Cardinal
# 152         Dat    Wasser       Neut      Dat 2.939115        Liter     Physical   0.1852983 0.6603248 -0.1171879      Yes
# Construction Kindattraction Measureattraction
# 152         NACa     -0.8096693        -0.7416931


# This extracts the estimate for the overall intercept.
coef(summary(glmm.03))['(Intercept)','Estimate']

# OUTPUT:
#
# [1] -3.652639

# Badness is a first-level effect. For the concrete data point,
# measure the coefficient by the value measured for the data
# point:
coef(summary(glmm.03))['Badness', 'Estimate'] *
  measure[99, 'Badness']

# OUTPUT:
#
# [1] 0.01828906


# Lookup the conditional mode for the apporpriate levels of
# the random effect. This if for Kindlemma:
ranef(glmm.03)$Kindlemma[
  as.character(measure[99, 'Kindlemma']), '(Intercept)']

# For each level of Kindlemma, there is a frequency value
# (the log lemma frequency per million words of the kind
# noun). It can be extracted much like a first-level effect
# (see for Badness above):
coef(summary(glmm.03))['Kindfreq', 'Estimate'] *
  measure[99, 'Kindfreq']

# OUTPUT:
#
# [1] -0.1593622


# Part II: Making model predictions "by hand".

# The following code is a complete calculation using the
# methods to extract single parameters and conditional modes
# from the model as demonstrated above.
data.99 <- measure[99,]                              # The measured data for item 99.
glmm.03.ranef <- ranef(glmm.03)                      # Conditional modes.
glmm.03.fixef <- coef(summary(glmm.03))[,'Estimate'] # Fixed effects.


# Factors are complicated because of the way estimates are
# presented in the model summary. This just creates a factor level
# name to be used later.
data.99.Measureclass <- paste0('Measureclass', unlist(lapply(data.99$Measureclass, as.character)))
data.99.Kindgender   <- paste0('Kindgender', unlist(lapply(data.99$Kindgender, as.character)))
data.99.Cardinal     <- paste0('Cardinal', unlist(lapply(data.99$Cardinal, as.character)))
data.99.Measurecase  <- paste0('Measurecase', unlist(lapply(data.99$Measurecase, as.character)))


# The value from the Measurelemma second-level model.
data.99.measurelevel <- unlist(unname(
  glmm.03.ranef$Measurelemma[as.character(data.99$Measurelemma),'(Intercept)'] +
    glmm.03.fixef['Measureattraction'] * data.99['Measureattraction']          +
    glmm.03.fixef['Measurefreq']       * data.99['Measurefreq']                +
    ifelse(data.99.Measureclass %in% names(glmm.03.fixef), glmm.03.fixef[data.99.Measureclass], 0)
))


# The value from the Kindlemma second-level model.
data.99.kindlevel <- unlist(unname(
  glmm.03.ranef$Kindlemma[as.character(data.99$Kindlemma),'(Intercept)']   +
    glmm.03.fixef['Kindattraction'] * data.99['Kindattraction']            +
    glmm.03.fixef['Kindfreq']       * data.99['Kindfreq']                  +
    ifelse(data.99.Kindgender %in% names(glmm.03.fixef), glmm.03.fixef[data.99.Kindgender], 0)
))


# Full model prediction.
data.99.predict.by.hand <- unlist(unname(
  glmm.03.fixef['(Intercept)']                                                             +
    data.99.measurelevel                                                                   +
    data.99.kindlevel                                                                      +
    glmm.03.fixef['Badness']   * data.99['Badness']                                        +
    glmm.03.fixef['Genitives'] * data.99['Genitives']                                      +
    ifelse(data.99.Cardinal %in% names(glmm.03.fixef), glmm.03.fixef[data.99.Cardinal], 0) +
    ifelse(data.99.Measurecase %in% names(glmm.03.fixef), glmm.03.fixef[data.99.Measurecase], 0)
))


# Compare with the built-in prediction function.

# Get a prediction from the GLMM for data point 99.
data.99.predict <- predict(glmm.03)[99]


print(invlogit(data.99.predict.by.hand))

# OUTPUT:
#
# [1] 0.03612644

print(invlogit(data.99.predict))

# OUTPUT:
#
# 152
# 0.03612644

coef(summary(glmm.03))['Kindfreq', 'Estimate'] *
  measure[99, 'Kindfreq']

# OUTPUT:
#
# [1] 0.4490978

# The same model with Measureclass as a nesting random effect.
glmm.04 <- glmer(Construction~1
                 +(1|Measurelemma)
                 +(1|Kindlemma)
                 +Badness
                 +Cardinal
                 +Genitives
                 +Measurecase
                 +Kindattraction
                 +Kindfreq
                 +Kindgender
                 +Measureattraction
                 +(1|Measureclass)
                 +Measurefreq
                 , data=measure, family=binomial(link=logit), na.action = na.fail,
                 control=glmerControl("bobyqa"))
glmm.04.r2 <- r.squaredGLMM(glmm.04)
print(summary(glmm.04))

# OUTPUT:
#
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Construction ~ 1 + (1 | Measurelemma) + (1 | Kindlemma) + Badness +
#   Cardinal + Genitives + Measurecase + Kindattraction + Kindfreq +
#   Kindgender + Measureattraction + (1 | Measureclass) + Measurefreq
# Data: measure
# Control: glmerControl("bobyqa")
#
# AIC      BIC   logLik deviance df.resid
# 3861.2   3959.1  -1915.6   3831.2     5048
#
# Scaled residuals:
#   Min      1Q  Median      3Q     Max
# -3.4496 -0.4355 -0.2159 -0.0595  9.4069
#
# Random effects:
#   Groups       Name        Variance Std.Dev.
# Measurelemma (Intercept) 0.2438   0.4938
# Kindlemma    (Intercept) 0.3642   0.6035
# Measureclass (Intercept) 0.1740   0.4171
# Number of obs: 5063, groups:  Measurelemma, 150; Kindlemma, 87; Measureclass, 5
#
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)       -3.13212    0.28447 -11.010  < 2e-16 ***
#   Badness           -0.15585    0.04730  -3.295 0.000984 ***
#   CardinalNo         1.24689    0.14582   8.551  < 2e-16 ***
#   Genitives         -0.70635    0.04614 -15.307  < 2e-16 ***
#   MeasurecaseAcc     0.02849    0.09453   0.301 0.763139
# MeasurecaseDat     0.71910    0.13014   5.525 3.29e-08 ***
#   Kindattraction     0.23510    0.08929   2.633 0.008464 **
#   Kindfreq           0.15024    0.08418   1.785 0.074308 .
# KindgenderNeut     0.02621    0.19572   0.134 0.893457
# KindgenderFem      1.30051    0.23713   5.484 4.15e-08 ***
#   Measureattraction  0.30995    0.08915   3.477 0.000508 ***
#   Measurefreq       -0.24156    0.07740  -3.121 0.001803 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Correlation of Fixed Effects:
#   (Intr) Badnss CrdnlN Gentvs MsrcsA MsrcsD Kndttr Kndfrq KndgnN KndgnF Msrttr
# Badness      0.004
# CardinalNo  -0.384  0.042
# Genitives    0.054 -0.043 -0.004
# MeasurcsAcc -0.167  0.004 -0.053 -0.055
# MeasurecsDt -0.219  0.003  0.047  0.020  0.405
# Kindattrctn  0.030  0.005  0.023  0.053 -0.004  0.021
# Kindfreq     0.081  0.000 -0.002 -0.005 -0.008 -0.008 -0.152
# KindgendrNt -0.360  0.004 -0.002  0.017 -0.018  0.001 -0.048 -0.202
# KindgendrFm -0.325 -0.014 -0.022 -0.052 -0.004  0.117 -0.335 -0.122  0.479
# Measrttrctn -0.087  0.041 -0.011 -0.025 -0.011  0.018 -0.069  0.005 -0.012  0.021
# Measurefreq  0.160 -0.018  0.068  0.020  0.012 -0.023 -0.034  0.058  0.008  0.007 -0.061

print(glmm.04.r2)

# OUTPUT:
#
# R2m       R2c
# 0.3446696 0.4705320
