Assessing Adaptation Of A Lentil (*Lens culinaris* Medik.) Diversity
Panel To Temperature And Photoperiod
================
Derek Michael Wright
2020-01-06

Contact: <derek.wright@usask.ca>

Derek Wright, Sandesh Neupane, Taryn Heidecker, Teketel Haile, Clarice
Coyne, Sripada Udupa, Eleonora Barilli, Diego Rubiales, Tania Gioia,
Reena Mehra, Ashutosh Sarker, Rajeev Dhakal, Babul Anwar, Debashish
Sarker, Albert Vandenberg, and Kirstin E. Bett

[View as
HTML](https://derekmichaelwright.github.io/AGILE_LDP_Phenology/README.html){:
.btn}

![](www/Logo_Agile.png)

# Introduction

**Project Collaborators**:

  - Department of Plant Sciences and Crop Development Centre, University
    of Saskatchewan, Saskatoon, Saskatchewan, Canada
  - United States Department of Agriculture Western Region Plant
    Introduction Station, Pullman, Washington, USA
  - International Center for Agriculture Research in the Dry Areas,
    Rabat, Morocco
  - Institute for Sustainable Agriculture, Spanish National Research
    Council, Cordoba, Spain
  - School of Agriculture, Forestry, Food and Environmental Sciences,
    University of Basilicata, Potenza, Italy
  - International Center for Agriculture Research in the Dry Areas, New
    Delhi, India
  - Local Initiatives for Biodiversity, Research and Development,
    Pokhara, Nepal
  - Bangladesh Agricultural Research Institute, Jessore, Bangladesh

**Project Sponsors**:

  - Saskatchewan Pulse Growers Association
  - Western Grains Research Foundation
  - GenomePrairie
  - GenomeCanada
  - Saskatchewan Ministry of Agriculture

# R Code Vignette

[Phenology\_Vignette.html](https://derekmichaelwright.github.io/AGILE_LDP_Phenology/Phenology_Vignette.html)

[Phenology\_Vignette.pdf](https://github.com/derekmichaelwright/AGILE_LDP_Phenology/blob/master/Phenology_Vignette.pdf)

# Shiny App

Download this folder and run `app.R` in `R`

![](www/shiny.png)

# Figures

## Figure 1

![](Figure_01_FieldTrialInfo.png)

## Figure 2

![](Figure_02_DataOverview.png)

## Figure 3

![](Figure_03_PCA.png)

## Figure 4

![](Figure_04_ModelPrediction.png)

## Figure 5

![](Figure_05_TbPc.png)

## Figure 6

![](Figure_06_OriginCoefficients.png)

## Figure 7

![](Figure_07_TempIncrease.png)

# Supplemental Figures

## Supplemental Figure 1

![](Supplemental_Figure_01_Scaling.png)

## Supplemental Figure 2

![](Supplemental_Figure_02_PercentFlowered.png)

## Supplemental Figure 3

![](Supplemental_Figure_03_Correlations.png)

## Supplemental Figure 4

![](Supplemental_Figure_04_Regressions.png)

## Supplemental Figure 5

![](Supplemental_Figure_05_Models.png)

## Supplemental Figure 6

![](Supplemental_Figure_06_CompareConstants.png)

## Supplemental Figure 7

![](Supplemental_Figure_07_TestModel.png)

## Supplemental Figure 8

![](Supplemental_Figure_08_ConstantsCompare.png)

## Supplemental Figure 9

![](Supplemental_Figure_09_TfPc.png)

## Supplemental Figure 10

![](Supplemental_Figure_10_Tf.png)

## Supplemental Figure 11

![](Supplemental_Figure_11_Pf.png)

# Supplemental Tables

## Supplemental Table 1

``` r
xx <- read.csv("Supplemental_Table_01.csv")
knitr::kable(xx[c(1:5,31:35,101:105,320:324),])
```

|     | Entry | Name              | Origin   | Source | Synonyms                     |
| --- | ----: | :---------------- | :------- | :----- | :--------------------------- |
| 1   |     1 | CDC Asterix AGL   | Canada   | USASK  |                              |
| 2   |     2 | CDC Rosie AGL     | Canada   | USASK  |                              |
| 3   |     3 | 3156-11 AGL       | Canada   | USASK  |                              |
| 4   |     4 | CDC Greenstar AGL | Canada   | USASK  |                              |
| 5   |     5 | CDC Cherie AGL    | Canada   | USASK  |                              |
| 31  |    31 | CN 105777 AGL     | Egypt    | PGRC   | LENS 170;B 47                |
| 32  |    32 | CN 105789 AGL     | Ethiopia | PGRC   | LENS 184;B 136               |
| 33  |    33 | CN 105791 AGL     | Egypt    | PGRC   | LENS 190;B 32                |
| 34  |    34 | CN 105862 AGL     | Tunisia  | PGRC   | LENS 559                     |
| 35  |    35 | CN 105863 AGL     | Tunisia  | PGRC   | LENS 561                     |
| 101 |   101 | ILL 6821 AGL      | Ethiopia | ICARDA | IG 73685;FLIP 89-63L;ALEMAYA |
| 102 |   102 | ILL 6853 AGL      | Syria    | ICARDA | IG 73717                     |
| 103 |   103 | ILL 7089 AGL      | Russia   | ICARDA | IG 73953                     |
| 104 |   104 | ILL 7558 AGL      | India    | ICARDA | IG 76277                     |
| 105 |   105 | ILL 7663 AGL      | ICARDA   | ICARDA | IG 114665;FLIP 95-7L         |
| 320 |   320 | W6 27754 LSP AGL  | USDA     | USDA   |                              |
| 321 |   321 | W6 27760 LSP AGL  | USDA     | USDA   |                              |
| 322 |   322 | W6 27763 LSP AGL  | USDA     | USDA   |                              |
| 323 |   323 | W6 27766 LSP AGL  | USDA     | USDA   |                              |
| 324 |   324 | W6 27767 LSP AGL  | USDA     | USDA   |                              |

## Supplemental Table 2

``` r
xx <- read.csv("Supplemental_Table_02.csv")
knitr::kable(xx)
```

| Location            | Year | Short.Name | Latitude |  Longitude | Planting.Date | Temperature..mean. | Photoperiod..mean. | Number.of.Seeds.Sown | Plot.Type           |
| :------------------ | ---: | :--------- | -------: | ---------: | :------------ | -----------------: | -----------------: | -------------------: | :------------------ |
| Sutherland, Canada  | 2016 | Su16       | 52.16770 | \-106.5054 | 2016-04-27    |               16.7 |               15.9 |                   60 | three, 1 meter rows |
| Rosthern, Canada    | 2016 | Ro16       | 52.68920 | \-106.2945 | 2016-05-06    |               17.2 |               16.2 |                   60 | three, 1 meter rows |
| Marchouch, Morocco  | 2016 | Mo16       | 33.62000 |   \-6.7200 | 2016-11-21    |               12.0 |               10.8 |                   25 | one, 1 meter row    |
| Cordoba, Spain      | 2016 | Sp16       | 37.90000 |   \-4.8000 | 2016-12-13    |               12.5 |               10.9 |                   25 | one, 1 meter row    |
| Metaponto, Italy    | 2016 | It16       | 40.39000 |    16.7800 | 2016-11-29    |               10.6 |               10.8 |                   25 | one, 1 meter row    |
| Bhopal, India       | 2016 | In16       | 23.11000 |    76.8800 | 2016-12-04    |               17.6 |               10.9 |                   25 | one, 1 meter row    |
| Bardiya, Nepal      | 2016 | Ne16       | 28.25000 |    81.5000 | 2016-11-14    |               19.2 |               11.0 |                   25 | one, 1 meter row    |
| Jessore, Bangladesh | 2016 | Ba16       | 23.19000 |    89.1900 | 2016-11-15    |               18.6 |               10.8 |                   25 | one, 1 meter row    |
| Sutherland, Canada  | 2017 | Su17       | 52.16832 | \-106.5108 | 2017-05-04    |               15.7 |               16.1 |                   70 | three, 1 meter rows |
| Rosthern, Canada    | 2017 | Ro17       | 52.69150 | \-106.2897 | 2017-05-19    |               17.5 |               16.4 |                   70 | three, 1 meter rows |
| Marchouch, Morocco  | 2017 | Mo17       | 33.62000 |   \-6.7200 | 2017-12-21    |               11.8 |               11.5 |                   50 | two, 1 meter rows   |
| Cordoba, Spain      | 2017 | Sp17       | 37.90000 |   \-4.8000 | 2017-12-14    |               11.7 |               11.1 |                   50 | two, 1 meter rows   |
| Metaponto, Italy    | 2017 | It17       | 40.39000 |    16.7800 | 2017-11-28    |               11.2 |               10.8 |                   50 | two, 1 meter rows   |
| Bhopal, India       | 2017 | In17       | 23.11500 |    76.8850 | 2017-11-09    |               20.6 |               10.7 |                   50 | two, 1 meter rows   |
| Jessore, Bangladesh | 2017 | Ba17       | 23.19500 |    89.1950 | 2017-12-03    |               21.7 |               11.0 |                   50 | two, 1 meter rows   |
| Central Ferry, USA  | 2018 | Us18       | 46.65000 | \-117.7600 | 2018-03-29    |               15.8 |               14.3 |                   50 | two, 1 meter rows   |
| Sutherland, Canada  | 2018 | Su18       | 52.16890 | \-106.5149 | 2018-05-09    |               17.6 |               16.1 |                   70 | three, 1 meter rows |
| Bardiya, Nepal      | 2017 | Ne17       | 28.42000 |    81.8600 | 2017-11-03    |               19.4 |               10.8 |                   50 | two, 1 meter rows   |

## Supplemental Table 3

``` r
xx <- read.csv("Supplemental_Table_03.csv")
knitr::kable(xx[c(1:2,61:62,201:202,647:648),])
```

|     | Entry | Name             |           a |           b |           c |           d |        RR | Environments | a\_p.value | b\_p.value | c\_p.value | d\_p.value |
| --- | ----: | :--------------- | ----------: | ----------: | ----------: | ----------: | --------: | -----------: | ---------: | ---------: | ---------: | ---------: |
| 1   |     1 | CDC Asterix AGL  | \-0.0187717 |   0.0003372 |   0.0020456 |          NA | 0.8978347 |           16 |  0.0000000 |  0.0000001 |  0.0000000 |         NA |
| 2   |     1 | CDC Asterix AGL  |   0.0072693 | \-0.0012017 | \-0.0003174 |   0.0001397 | 0.8992827 |           16 |  0.6202032 |  0.1688352 |  0.8110850 |  0.0793975 |
| 61  |    31 | CN 105777 AGL    | \-0.0203402 |   0.0006260 |   0.0020791 |          NA | 0.8417776 |           18 |  0.0000000 |  0.0000000 |  0.0000000 |         NA |
| 62  |    31 | CN 105777 AGL    | \-0.0299805 |   0.0011958 |   0.0029539 | \-0.0000518 | 0.8473458 |           18 |  0.1417083 |  0.3178702 |  0.1107758 |  0.6321050 |
| 201 |   101 | ILL 6821 AGL     | \-0.0207765 |   0.0001801 |   0.0024844 |          NA | 0.9055885 |           14 |  0.0000000 |  0.0006591 |  0.0000000 |         NA |
| 202 |   101 | ILL 6821 AGL     | \-0.0149886 | \-0.0001621 |   0.0019592 |   0.0000311 | 0.9051060 |           14 |  0.2937562 |  0.8467141 |  0.1321410 |  0.6828037 |
| 647 |   324 | W6 27767 LSP AGL | \-0.0194010 |   0.0002353 |   0.0022805 |          NA | 0.8697454 |           15 |  0.0000000 |  0.0000289 |  0.0000000 |         NA |
| 648 |   324 | W6 27767 LSP AGL | \-0.0080395 | \-0.0004354 |   0.0012494 |   0.0000609 | 0.8679062 |           15 |  0.5871621 |  0.6178443 |  0.3531932 |  0.4421900 |

## Supplemental Table 4

``` r
xx <- read.csv("Supplemental_Table_04.csv")
knitr::kable(rbind(head(xx), tail(xx)))
```

|     | Temperate\_Location | SouthAsian\_Location | Mediterranean\_Location |       RR | Genotypes |
| --- | :------------------ | :------------------- | :---------------------- | -------: | --------: |
| 1   | Ro17                | In16                 | Sp16                    | 0.461770 |       159 |
| 2   | Su18                | In16                 | Sp16                    | 0.462242 |       159 |
| 3   | Ro16                | In16                 | Sp16                    | 0.466809 |       159 |
| 4   | Su17                | In16                 | Sp16                    | 0.469932 |       159 |
| 5   | Su16                | In16                 | Sp16                    | 0.473691 |       159 |
| 6   | Ro17                | In16                 | It17                    | 0.475920 |       159 |
| 211 | Ro17                | Ba17                 | Sp17                    | 0.858843 |       291 |
| 212 | Su16                | Ba17                 | Mo16                    | 0.858923 |       291 |
| 213 | Ro16                | Ba17                 | Sp17                    | 0.859936 |       291 |
| 214 | Us18                | Ba17                 | Sp17                    | 0.861168 |       289 |
| 215 | Su17                | Ba17                 | Sp17                    | 0.862977 |       291 |
| 216 | Su16                | Ba17                 | Sp17                    | 0.863054 |       291 |

# Animations

## Temperature and Photoperiod Regressions

## Model Predictions

## Photothermal Planes

# Additional Figures

## Additional Figure 1

![](Additional/Additional_Figure_01_LDPOriginMap.png)

## Additional Figure 2

![](Additional/Additional_Figure_02_DTFDTSDTM.png)

## Additional Figure 3

![](Additional/Additional_Figure_03_MacroEnvPhenology.png)

## Additional Figure 4

![](Additional/Additional_Figure_04_ggridges.png)

## Additional Figure 5

[Additional/Additional\_Figure\_05\_PCA\_3D.html](https://derekmichaelwright.github.io/AGILE_LDP_Phenology/Additional/Additional_Figure_05_PCA_3D.html)

## Additional Figure 6

![](Additional/Additional_Figure_06_PCA.png)

## Additional Figure 7

![](Additional/Additional_Figure_07_DTFByCluster.png)

## Additional Figure 8

![](Additional/Additional_Figure_08_ClusterOrigins.png)

## Additional Figure 9

![](Additional/Additional_Figure_09_LDPOriginsByCluster.png)

## Additional Figure 10

![](Additional/Additional_Figure_10_TxPRR.png)

## Additional Figure 11

![](Additional/Additional_Figure_11_Coefs.png)

## Additional Figure 12

![](Additional/Additional_Figure_12_CoefsP.png)

## Additional Figure 13

![](Additional/Additional_Figure_13_bP.png)

## Additional Figure 14

![](Additional/Additional_Figure_14_PTT.png)
