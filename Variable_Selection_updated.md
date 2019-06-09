Variable Selection from Heart Disease Data
================
Tapas Mishra
16 April 2019

``` r
# Load the data

heart.data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')
names(heart.data) <- c( "age", "sex", "chest_pain_type", "resting_blood_pressure", "cholesterol","fasting_blood_sugar", "rest_ecg",
                   "max_heart_rate_achieved","exercise_induced_angina", "st_depression","st_slope", "num_major_vessels", "thalassemia", "target")

str(heart.data)
```

    ## 'data.frame':    303 obs. of  14 variables:
    ##  $ age                    : num  63 67 67 37 41 56 62 57 63 53 ...
    ##  $ sex                    : num  1 1 1 1 0 1 0 0 1 1 ...
    ##  $ chest_pain_type        : num  1 4 4 3 2 2 4 4 4 4 ...
    ##  $ resting_blood_pressure : num  145 160 120 130 130 120 140 120 130 140 ...
    ##  $ cholesterol            : num  233 286 229 250 204 236 268 354 254 203 ...
    ##  $ fasting_blood_sugar    : num  1 0 0 0 0 0 0 0 0 1 ...
    ##  $ rest_ecg               : num  2 2 2 0 2 0 2 0 2 2 ...
    ##  $ max_heart_rate_achieved: num  150 108 129 187 172 178 160 163 147 155 ...
    ##  $ exercise_induced_angina: num  0 1 1 0 0 0 0 1 0 1 ...
    ##  $ st_depression          : num  2.3 1.5 2.6 3.5 1.4 0.8 3.6 0.6 1.4 3.1 ...
    ##  $ st_slope               : num  3 2 2 3 1 1 3 1 2 3 ...
    ##  $ num_major_vessels      : num  0 3 2 0 0 0 2 0 1 0 ...
    ##  $ thalassemia            : num  6 3 7 3 3 3 3 3 7 7 ...
    ##  $ target                 : int  0 2 1 0 0 0 3 0 2 1 ...

``` r
summary(heart.data)
```

    ##       age             sex         chest_pain_type resting_blood_pressure
    ##  Min.   :29.00   Min.   :0.0000   Min.   :1.000   Min.   : 94.0         
    ##  1st Qu.:48.00   1st Qu.:0.0000   1st Qu.:3.000   1st Qu.:120.0         
    ##  Median :56.00   Median :1.0000   Median :3.000   Median :130.0         
    ##  Mean   :54.44   Mean   :0.6799   Mean   :3.158   Mean   :131.7         
    ##  3rd Qu.:61.00   3rd Qu.:1.0000   3rd Qu.:4.000   3rd Qu.:140.0         
    ##  Max.   :77.00   Max.   :1.0000   Max.   :4.000   Max.   :200.0         
    ##                                                                         
    ##   cholesterol    fasting_blood_sugar    rest_ecg     
    ##  Min.   :126.0   Min.   :0.0000      Min.   :0.0000  
    ##  1st Qu.:211.0   1st Qu.:0.0000      1st Qu.:0.0000  
    ##  Median :241.0   Median :0.0000      Median :1.0000  
    ##  Mean   :246.7   Mean   :0.1485      Mean   :0.9901  
    ##  3rd Qu.:275.0   3rd Qu.:0.0000      3rd Qu.:2.0000  
    ##  Max.   :564.0   Max.   :1.0000      Max.   :2.0000  
    ##                                                      
    ##  max_heart_rate_achieved exercise_induced_angina st_depression 
    ##  Min.   : 71.0           Min.   :0.0000          Min.   :0.00  
    ##  1st Qu.:133.5           1st Qu.:0.0000          1st Qu.:0.00  
    ##  Median :153.0           Median :0.0000          Median :0.80  
    ##  Mean   :149.6           Mean   :0.3267          Mean   :1.04  
    ##  3rd Qu.:166.0           3rd Qu.:1.0000          3rd Qu.:1.60  
    ##  Max.   :202.0           Max.   :1.0000          Max.   :6.20  
    ##                                                                
    ##     st_slope     num_major_vessels  thalassemia        target      
    ##  Min.   :1.000   Min.   :0.0000    Min.   :3.000   Min.   :0.0000  
    ##  1st Qu.:1.000   1st Qu.:0.0000    1st Qu.:3.000   1st Qu.:0.0000  
    ##  Median :2.000   Median :0.0000    Median :3.000   Median :0.0000  
    ##  Mean   :1.601   Mean   :0.6722    Mean   :4.734   Mean   :0.9373  
    ##  3rd Qu.:2.000   3rd Qu.:1.0000    3rd Qu.:7.000   3rd Qu.:2.0000  
    ##  Max.   :3.000   Max.   :3.0000    Max.   :7.000   Max.   :4.0000  
    ##                  NA's   :4         NA's   :2

``` r
heart.data <- na.omit(heart.data)
```

I’m also going to change the values of the categorical variables, to
improve the interpretation later on,

``` r
heart.data$chest_pain_type <- factor(heart.data$chest_pain_type,
                    levels = c(1,2,3,4),
                    labels = c("typical angina", "atypical angina","non-anginal pain","asymptomatic"))




heart.data$target[heart.data$target > 0] <- 1

heart.data$target <- factor(heart.data$target,
                    levels = c(0,1),
                    labels = c("No", "Yes"))
```

Check the data now ..

``` r
summary(heart.data)
```

    ##       age             sex                 chest_pain_type
    ##  Min.   :29.00   Min.   :0.0000   typical angina  : 23   
    ##  1st Qu.:48.00   1st Qu.:0.0000   atypical angina : 49   
    ##  Median :56.00   Median :1.0000   non-anginal pain: 83   
    ##  Mean   :54.54   Mean   :0.6768   asymptomatic    :142   
    ##  3rd Qu.:61.00   3rd Qu.:1.0000                          
    ##  Max.   :77.00   Max.   :1.0000                          
    ##  resting_blood_pressure  cholesterol    fasting_blood_sugar
    ##  Min.   : 94.0          Min.   :126.0   Min.   :0.0000     
    ##  1st Qu.:120.0          1st Qu.:211.0   1st Qu.:0.0000     
    ##  Median :130.0          Median :243.0   Median :0.0000     
    ##  Mean   :131.7          Mean   :247.4   Mean   :0.1448     
    ##  3rd Qu.:140.0          3rd Qu.:276.0   3rd Qu.:0.0000     
    ##  Max.   :200.0          Max.   :564.0   Max.   :1.0000     
    ##     rest_ecg      max_heart_rate_achieved exercise_induced_angina
    ##  Min.   :0.0000   Min.   : 71.0           Min.   :0.0000         
    ##  1st Qu.:0.0000   1st Qu.:133.0           1st Qu.:0.0000         
    ##  Median :1.0000   Median :153.0           Median :0.0000         
    ##  Mean   :0.9966   Mean   :149.6           Mean   :0.3266         
    ##  3rd Qu.:2.0000   3rd Qu.:166.0           3rd Qu.:1.0000         
    ##  Max.   :2.0000   Max.   :202.0           Max.   :1.0000         
    ##  st_depression      st_slope     num_major_vessels  thalassemia   
    ##  Min.   :0.000   Min.   :1.000   Min.   :0.0000    Min.   :3.000  
    ##  1st Qu.:0.000   1st Qu.:1.000   1st Qu.:0.0000    1st Qu.:3.000  
    ##  Median :0.800   Median :2.000   Median :0.0000    Median :3.000  
    ##  Mean   :1.056   Mean   :1.603   Mean   :0.6768    Mean   :4.731  
    ##  3rd Qu.:1.600   3rd Qu.:2.000   3rd Qu.:1.0000    3rd Qu.:7.000  
    ##  Max.   :6.200   Max.   :3.000   Max.   :3.0000    Max.   :7.000  
    ##  target   
    ##  No :160  
    ##  Yes:137  
    ##           
    ##           
    ##           
    ## 

Producing violin plots for distribution of contineous
    features

``` r
library(vioplot)
```

    ## Loading required package: sm

    ## Package 'sm', version 2.2-5.6: type help(sm) for summary information

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
par(mfrow = c(2,2))
with(heart.data,vioplot(age,names="Age",horizontal=TRUE, col = "orange", main = "Distribution of Age"))
with(heart.data,vioplot(resting_blood_pressure  ,names="Resting Blood Pressure (Trestbps)",horizontal=TRUE, col = "orange", main = "Distribution of Resting Blood Pressure"))
with(heart.data,vioplot(cholesterol ,names="Cholesterol  (chol)",horizontal=TRUE, col = "orange", main = "Distribution of Cholesterol"))
with(heart.data,vioplot(max_heart_rate_achieved  ,names="Maximum Heart Rate Achieved (Thalch)",horizontal=TRUE, col = "orange", main = "Distribution of Maximum Heart Rate Achieved"))
```

![](Variable_Selection_updated_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Creating a pair
plot

``` r
plot(heart.data)
```

![](Variable_Selection_updated_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Checking the multicollinearity using
    VIF

``` r
round(diag(solve(cor(heart.data[,c(1,2,4,5,6,7,8,9,10,11)]))),2)
```

    ##                     age                     sex  resting_blood_pressure 
    ##                    1.39                    1.10                    1.18 
    ##             cholesterol     fasting_blood_sugar                rest_ecg 
    ##                    1.13                    1.05                    1.08 
    ## max_heart_rate_achieved exercise_induced_angina           st_depression 
    ##                    1.57                    1.25                    1.64 
    ##                st_slope 
    ##                    1.63

``` r
heart.data
```

    ##     age sex  chest_pain_type resting_blood_pressure cholesterol
    ## 1    63   1   typical angina                    145         233
    ## 2    67   1     asymptomatic                    160         286
    ## 3    67   1     asymptomatic                    120         229
    ## 4    37   1 non-anginal pain                    130         250
    ## 5    41   0  atypical angina                    130         204
    ## 6    56   1  atypical angina                    120         236
    ## 7    62   0     asymptomatic                    140         268
    ## 8    57   0     asymptomatic                    120         354
    ## 9    63   1     asymptomatic                    130         254
    ## 10   53   1     asymptomatic                    140         203
    ## 11   57   1     asymptomatic                    140         192
    ## 12   56   0  atypical angina                    140         294
    ## 13   56   1 non-anginal pain                    130         256
    ## 14   44   1  atypical angina                    120         263
    ## 15   52   1 non-anginal pain                    172         199
    ## 16   57   1 non-anginal pain                    150         168
    ## 17   48   1  atypical angina                    110         229
    ## 18   54   1     asymptomatic                    140         239
    ## 19   48   0 non-anginal pain                    130         275
    ## 20   49   1  atypical angina                    130         266
    ## 21   64   1   typical angina                    110         211
    ## 22   58   0   typical angina                    150         283
    ## 23   58   1  atypical angina                    120         284
    ## 24   58   1 non-anginal pain                    132         224
    ## 25   60   1     asymptomatic                    130         206
    ## 26   50   0 non-anginal pain                    120         219
    ## 27   58   0 non-anginal pain                    120         340
    ## 28   66   0   typical angina                    150         226
    ## 29   43   1     asymptomatic                    150         247
    ## 30   40   1     asymptomatic                    110         167
    ## 31   69   0   typical angina                    140         239
    ## 32   60   1     asymptomatic                    117         230
    ## 33   64   1 non-anginal pain                    140         335
    ## 34   59   1     asymptomatic                    135         234
    ## 35   44   1 non-anginal pain                    130         233
    ## 36   42   1     asymptomatic                    140         226
    ## 37   43   1     asymptomatic                    120         177
    ## 38   57   1     asymptomatic                    150         276
    ## 39   55   1     asymptomatic                    132         353
    ## 40   61   1 non-anginal pain                    150         243
    ## 41   65   0     asymptomatic                    150         225
    ## 42   40   1   typical angina                    140         199
    ## 43   71   0  atypical angina                    160         302
    ## 44   59   1 non-anginal pain                    150         212
    ## 45   61   0     asymptomatic                    130         330
    ## 46   58   1 non-anginal pain                    112         230
    ## 47   51   1 non-anginal pain                    110         175
    ## 48   50   1     asymptomatic                    150         243
    ## 49   65   0 non-anginal pain                    140         417
    ## 50   53   1 non-anginal pain                    130         197
    ## 51   41   0  atypical angina                    105         198
    ## 52   65   1     asymptomatic                    120         177
    ## 53   44   1     asymptomatic                    112         290
    ## 54   44   1  atypical angina                    130         219
    ## 55   60   1     asymptomatic                    130         253
    ## 56   54   1     asymptomatic                    124         266
    ## 57   50   1 non-anginal pain                    140         233
    ## 58   41   1     asymptomatic                    110         172
    ## 59   54   1 non-anginal pain                    125         273
    ## 60   51   1   typical angina                    125         213
    ## 61   51   0     asymptomatic                    130         305
    ## 62   46   0 non-anginal pain                    142         177
    ## 63   58   1     asymptomatic                    128         216
    ## 64   54   0 non-anginal pain                    135         304
    ## 65   54   1     asymptomatic                    120         188
    ## 66   60   1     asymptomatic                    145         282
    ## 67   60   1 non-anginal pain                    140         185
    ## 68   54   1 non-anginal pain                    150         232
    ## 69   59   1     asymptomatic                    170         326
    ## 70   46   1 non-anginal pain                    150         231
    ## 71   65   0 non-anginal pain                    155         269
    ## 72   67   1     asymptomatic                    125         254
    ## 73   62   1     asymptomatic                    120         267
    ## 74   65   1     asymptomatic                    110         248
    ## 75   44   1     asymptomatic                    110         197
    ## 76   65   0 non-anginal pain                    160         360
    ## 77   60   1     asymptomatic                    125         258
    ## 78   51   0 non-anginal pain                    140         308
    ## 79   48   1  atypical angina                    130         245
    ## 80   58   1     asymptomatic                    150         270
    ## 81   45   1     asymptomatic                    104         208
    ## 82   53   0     asymptomatic                    130         264
    ## 83   39   1 non-anginal pain                    140         321
    ## 84   68   1 non-anginal pain                    180         274
    ## 85   52   1  atypical angina                    120         325
    ## 86   44   1 non-anginal pain                    140         235
    ## 87   47   1 non-anginal pain                    138         257
    ## 89   53   0     asymptomatic                    138         234
    ## 90   51   0 non-anginal pain                    130         256
    ## 91   66   1     asymptomatic                    120         302
    ## 92   62   0     asymptomatic                    160         164
    ## 93   62   1 non-anginal pain                    130         231
    ## 94   44   0 non-anginal pain                    108         141
    ## 95   63   0 non-anginal pain                    135         252
    ## 96   52   1     asymptomatic                    128         255
    ## 97   59   1     asymptomatic                    110         239
    ## 98   60   0     asymptomatic                    150         258
    ## 99   52   1  atypical angina                    134         201
    ## 100  48   1     asymptomatic                    122         222
    ## 101  45   1     asymptomatic                    115         260
    ## 102  34   1   typical angina                    118         182
    ## 103  57   0     asymptomatic                    128         303
    ## 104  71   0 non-anginal pain                    110         265
    ## 105  49   1 non-anginal pain                    120         188
    ## 106  54   1  atypical angina                    108         309
    ## 107  59   1     asymptomatic                    140         177
    ## 108  57   1 non-anginal pain                    128         229
    ## 109  61   1     asymptomatic                    120         260
    ## 110  39   1     asymptomatic                    118         219
    ## 111  61   0     asymptomatic                    145         307
    ## 112  56   1     asymptomatic                    125         249
    ## 113  52   1   typical angina                    118         186
    ## 114  43   0     asymptomatic                    132         341
    ## 115  62   0 non-anginal pain                    130         263
    ## 116  41   1  atypical angina                    135         203
    ## 117  58   1 non-anginal pain                    140         211
    ## 118  35   0     asymptomatic                    138         183
    ## 119  63   1     asymptomatic                    130         330
    ## 120  65   1     asymptomatic                    135         254
    ## 121  48   1     asymptomatic                    130         256
    ## 122  63   0     asymptomatic                    150         407
    ## 123  51   1 non-anginal pain                    100         222
    ## 124  55   1     asymptomatic                    140         217
    ## 125  65   1   typical angina                    138         282
    ## 126  45   0  atypical angina                    130         234
    ## 127  56   0     asymptomatic                    200         288
    ## 128  54   1     asymptomatic                    110         239
    ## 129  44   1  atypical angina                    120         220
    ## 130  62   0     asymptomatic                    124         209
    ## 131  54   1 non-anginal pain                    120         258
    ## 132  51   1 non-anginal pain                     94         227
    ## 133  29   1  atypical angina                    130         204
    ## 134  51   1     asymptomatic                    140         261
    ## 135  43   0 non-anginal pain                    122         213
    ## 136  55   0  atypical angina                    135         250
    ## 137  70   1     asymptomatic                    145         174
    ## 138  62   1  atypical angina                    120         281
    ## 139  35   1     asymptomatic                    120         198
    ## 140  51   1 non-anginal pain                    125         245
    ## 141  59   1  atypical angina                    140         221
    ## 142  59   1   typical angina                    170         288
    ## 143  52   1  atypical angina                    128         205
    ## 144  64   1 non-anginal pain                    125         309
    ## 145  58   1 non-anginal pain                    105         240
    ## 146  47   1 non-anginal pain                    108         243
    ## 147  57   1     asymptomatic                    165         289
    ## 148  41   1 non-anginal pain                    112         250
    ## 149  45   1  atypical angina                    128         308
    ## 150  60   0 non-anginal pain                    102         318
    ## 151  52   1   typical angina                    152         298
    ## 152  42   0     asymptomatic                    102         265
    ## 153  67   0 non-anginal pain                    115         564
    ## 154  55   1     asymptomatic                    160         289
    ## 155  64   1     asymptomatic                    120         246
    ## 156  70   1     asymptomatic                    130         322
    ## 157  51   1     asymptomatic                    140         299
    ## 158  58   1     asymptomatic                    125         300
    ## 159  60   1     asymptomatic                    140         293
    ## 160  68   1 non-anginal pain                    118         277
    ## 161  46   1  atypical angina                    101         197
    ## 162  77   1     asymptomatic                    125         304
    ## 163  54   0 non-anginal pain                    110         214
    ## 164  58   0     asymptomatic                    100         248
    ## 165  48   1 non-anginal pain                    124         255
    ## 166  57   1     asymptomatic                    132         207
    ## 168  54   0  atypical angina                    132         288
    ## 169  35   1     asymptomatic                    126         282
    ## 170  45   0  atypical angina                    112         160
    ## 171  70   1 non-anginal pain                    160         269
    ## 172  53   1     asymptomatic                    142         226
    ## 173  59   0     asymptomatic                    174         249
    ## 174  62   0     asymptomatic                    140         394
    ## 175  64   1     asymptomatic                    145         212
    ## 176  57   1     asymptomatic                    152         274
    ## 177  52   1     asymptomatic                    108         233
    ## 178  56   1     asymptomatic                    132         184
    ## 179  43   1 non-anginal pain                    130         315
    ## 180  53   1 non-anginal pain                    130         246
    ## 181  48   1     asymptomatic                    124         274
    ## 182  56   0     asymptomatic                    134         409
    ## 183  42   1   typical angina                    148         244
    ## 184  59   1   typical angina                    178         270
    ## 185  60   0     asymptomatic                    158         305
    ## 186  63   0  atypical angina                    140         195
    ## 187  42   1 non-anginal pain                    120         240
    ## 188  66   1  atypical angina                    160         246
    ## 189  54   1  atypical angina                    192         283
    ## 190  69   1 non-anginal pain                    140         254
    ## 191  50   1 non-anginal pain                    129         196
    ## 192  51   1     asymptomatic                    140         298
    ## 194  62   0     asymptomatic                    138         294
    ## 195  68   0 non-anginal pain                    120         211
    ## 196  67   1     asymptomatic                    100         299
    ## 197  69   1   typical angina                    160         234
    ## 198  45   0     asymptomatic                    138         236
    ## 199  50   0  atypical angina                    120         244
    ## 200  59   1   typical angina                    160         273
    ## 201  50   0     asymptomatic                    110         254
    ## 202  64   0     asymptomatic                    180         325
    ## 203  57   1 non-anginal pain                    150         126
    ## 204  64   0 non-anginal pain                    140         313
    ## 205  43   1     asymptomatic                    110         211
    ## 206  45   1     asymptomatic                    142         309
    ## 207  58   1     asymptomatic                    128         259
    ## 208  50   1     asymptomatic                    144         200
    ## 209  55   1  atypical angina                    130         262
    ## 210  62   0     asymptomatic                    150         244
    ## 211  37   0 non-anginal pain                    120         215
    ## 212  38   1   typical angina                    120         231
    ## 213  41   1 non-anginal pain                    130         214
    ## 214  66   0     asymptomatic                    178         228
    ## 215  52   1     asymptomatic                    112         230
    ## 216  56   1   typical angina                    120         193
    ## 217  46   0  atypical angina                    105         204
    ## 218  46   0     asymptomatic                    138         243
    ## 219  64   0     asymptomatic                    130         303
    ## 220  59   1     asymptomatic                    138         271
    ## 221  41   0 non-anginal pain                    112         268
    ## 222  54   0 non-anginal pain                    108         267
    ## 223  39   0 non-anginal pain                     94         199
    ## 224  53   1     asymptomatic                    123         282
    ## 225  63   0     asymptomatic                    108         269
    ## 226  34   0  atypical angina                    118         210
    ## 227  47   1     asymptomatic                    112         204
    ## 228  67   0 non-anginal pain                    152         277
    ## 229  54   1     asymptomatic                    110         206
    ## 230  66   1     asymptomatic                    112         212
    ## 231  52   0 non-anginal pain                    136         196
    ## 232  55   0     asymptomatic                    180         327
    ## 233  49   1 non-anginal pain                    118         149
    ## 234  74   0  atypical angina                    120         269
    ## 235  54   0 non-anginal pain                    160         201
    ## 236  54   1     asymptomatic                    122         286
    ## 237  56   1     asymptomatic                    130         283
    ## 238  46   1     asymptomatic                    120         249
    ## 239  49   0  atypical angina                    134         271
    ## 240  42   1  atypical angina                    120         295
    ## 241  41   1  atypical angina                    110         235
    ## 242  41   0  atypical angina                    126         306
    ## 243  49   0     asymptomatic                    130         269
    ## 244  61   1   typical angina                    134         234
    ## 245  60   0 non-anginal pain                    120         178
    ## 246  67   1     asymptomatic                    120         237
    ## 247  58   1     asymptomatic                    100         234
    ## 248  47   1     asymptomatic                    110         275
    ## 249  52   1     asymptomatic                    125         212
    ## 250  62   1  atypical angina                    128         208
    ## 251  57   1     asymptomatic                    110         201
    ## 252  58   1     asymptomatic                    146         218
    ## 253  64   1     asymptomatic                    128         263
    ## 254  51   0 non-anginal pain                    120         295
    ## 255  43   1     asymptomatic                    115         303
    ## 256  42   0 non-anginal pain                    120         209
    ## 257  67   0     asymptomatic                    106         223
    ## 258  76   0 non-anginal pain                    140         197
    ## 259  70   1  atypical angina                    156         245
    ## 260  57   1  atypical angina                    124         261
    ## 261  44   0 non-anginal pain                    118         242
    ## 262  58   0  atypical angina                    136         319
    ## 263  60   0   typical angina                    150         240
    ## 264  44   1 non-anginal pain                    120         226
    ## 265  61   1     asymptomatic                    138         166
    ## 266  42   1     asymptomatic                    136         315
    ## 268  59   1 non-anginal pain                    126         218
    ## 269  40   1     asymptomatic                    152         223
    ## 270  42   1 non-anginal pain                    130         180
    ## 271  61   1     asymptomatic                    140         207
    ## 272  66   1     asymptomatic                    160         228
    ## 273  46   1     asymptomatic                    140         311
    ## 274  71   0     asymptomatic                    112         149
    ## 275  59   1   typical angina                    134         204
    ## 276  64   1   typical angina                    170         227
    ## 277  66   0 non-anginal pain                    146         278
    ## 278  39   0 non-anginal pain                    138         220
    ## 279  57   1  atypical angina                    154         232
    ## 280  58   0     asymptomatic                    130         197
    ## 281  57   1     asymptomatic                    110         335
    ## 282  47   1 non-anginal pain                    130         253
    ## 283  55   0     asymptomatic                    128         205
    ## 284  35   1  atypical angina                    122         192
    ## 285  61   1     asymptomatic                    148         203
    ## 286  58   1     asymptomatic                    114         318
    ## 287  58   0     asymptomatic                    170         225
    ## 289  56   1  atypical angina                    130         221
    ## 290  56   1  atypical angina                    120         240
    ## 291  67   1 non-anginal pain                    152         212
    ## 292  55   0  atypical angina                    132         342
    ## 293  44   1     asymptomatic                    120         169
    ## 294  63   1     asymptomatic                    140         187
    ## 295  63   0     asymptomatic                    124         197
    ## 296  41   1  atypical angina                    120         157
    ## 297  59   1     asymptomatic                    164         176
    ## 298  57   0     asymptomatic                    140         241
    ## 299  45   1   typical angina                    110         264
    ## 300  68   1     asymptomatic                    144         193
    ## 301  57   1     asymptomatic                    130         131
    ## 302  57   0  atypical angina                    130         236
    ##     fasting_blood_sugar rest_ecg max_heart_rate_achieved
    ## 1                     1        2                     150
    ## 2                     0        2                     108
    ## 3                     0        2                     129
    ## 4                     0        0                     187
    ## 5                     0        2                     172
    ## 6                     0        0                     178
    ## 7                     0        2                     160
    ## 8                     0        0                     163
    ## 9                     0        2                     147
    ## 10                    1        2                     155
    ## 11                    0        0                     148
    ## 12                    0        2                     153
    ## 13                    1        2                     142
    ## 14                    0        0                     173
    ## 15                    1        0                     162
    ## 16                    0        0                     174
    ## 17                    0        0                     168
    ## 18                    0        0                     160
    ## 19                    0        0                     139
    ## 20                    0        0                     171
    ## 21                    0        2                     144
    ## 22                    1        2                     162
    ## 23                    0        2                     160
    ## 24                    0        2                     173
    ## 25                    0        2                     132
    ## 26                    0        0                     158
    ## 27                    0        0                     172
    ## 28                    0        0                     114
    ## 29                    0        0                     171
    ## 30                    0        2                     114
    ## 31                    0        0                     151
    ## 32                    1        0                     160
    ## 33                    0        0                     158
    ## 34                    0        0                     161
    ## 35                    0        0                     179
    ## 36                    0        0                     178
    ## 37                    0        2                     120
    ## 38                    0        2                     112
    ## 39                    0        0                     132
    ## 40                    1        0                     137
    ## 41                    0        2                     114
    ## 42                    0        0                     178
    ## 43                    0        0                     162
    ## 44                    1        0                     157
    ## 45                    0        2                     169
    ## 46                    0        2                     165
    ## 47                    0        0                     123
    ## 48                    0        2                     128
    ## 49                    1        2                     157
    ## 50                    1        2                     152
    ## 51                    0        0                     168
    ## 52                    0        0                     140
    ## 53                    0        2                     153
    ## 54                    0        2                     188
    ## 55                    0        0                     144
    ## 56                    0        2                     109
    ## 57                    0        0                     163
    ## 58                    0        2                     158
    ## 59                    0        2                     152
    ## 60                    0        2                     125
    ## 61                    0        0                     142
    ## 62                    0        2                     160
    ## 63                    0        2                     131
    ## 64                    1        0                     170
    ## 65                    0        0                     113
    ## 66                    0        2                     142
    ## 67                    0        2                     155
    ## 68                    0        2                     165
    ## 69                    0        2                     140
    ## 70                    0        0                     147
    ## 71                    0        0                     148
    ## 72                    1        0                     163
    ## 73                    0        0                      99
    ## 74                    0        2                     158
    ## 75                    0        2                     177
    ## 76                    0        2                     151
    ## 77                    0        2                     141
    ## 78                    0        2                     142
    ## 79                    0        2                     180
    ## 80                    0        2                     111
    ## 81                    0        2                     148
    ## 82                    0        2                     143
    ## 83                    0        2                     182
    ## 84                    1        2                     150
    ## 85                    0        0                     172
    ## 86                    0        2                     180
    ## 87                    0        2                     156
    ## 89                    0        2                     160
    ## 90                    0        2                     149
    ## 91                    0        2                     151
    ## 92                    0        2                     145
    ## 93                    0        0                     146
    ## 94                    0        0                     175
    ## 95                    0        2                     172
    ## 96                    0        0                     161
    ## 97                    0        2                     142
    ## 98                    0        2                     157
    ## 99                    0        0                     158
    ## 100                   0        2                     186
    ## 101                   0        2                     185
    ## 102                   0        2                     174
    ## 103                   0        2                     159
    ## 104                   1        2                     130
    ## 105                   0        0                     139
    ## 106                   0        0                     156
    ## 107                   0        0                     162
    ## 108                   0        2                     150
    ## 109                   0        0                     140
    ## 110                   0        0                     140
    ## 111                   0        2                     146
    ## 112                   1        2                     144
    ## 113                   0        2                     190
    ## 114                   1        2                     136
    ## 115                   0        0                      97
    ## 116                   0        0                     132
    ## 117                   1        2                     165
    ## 118                   0        0                     182
    ## 119                   1        2                     132
    ## 120                   0        2                     127
    ## 121                   1        2                     150
    ## 122                   0        2                     154
    ## 123                   0        0                     143
    ## 124                   0        0                     111
    ## 125                   1        2                     174
    ## 126                   0        2                     175
    ## 127                   1        2                     133
    ## 128                   0        0                     126
    ## 129                   0        0                     170
    ## 130                   0        0                     163
    ## 131                   0        2                     147
    ## 132                   0        0                     154
    ## 133                   0        2                     202
    ## 134                   0        2                     186
    ## 135                   0        0                     165
    ## 136                   0        2                     161
    ## 137                   0        0                     125
    ## 138                   0        2                     103
    ## 139                   0        0                     130
    ## 140                   1        2                     166
    ## 141                   0        0                     164
    ## 142                   0        2                     159
    ## 143                   1        0                     184
    ## 144                   0        0                     131
    ## 145                   0        2                     154
    ## 146                   0        0                     152
    ## 147                   1        2                     124
    ## 148                   0        0                     179
    ## 149                   0        2                     170
    ## 150                   0        0                     160
    ## 151                   1        0                     178
    ## 152                   0        2                     122
    ## 153                   0        2                     160
    ## 154                   0        2                     145
    ## 155                   0        2                      96
    ## 156                   0        2                     109
    ## 157                   0        0                     173
    ## 158                   0        2                     171
    ## 159                   0        2                     170
    ## 160                   0        0                     151
    ## 161                   1        0                     156
    ## 162                   0        2                     162
    ## 163                   0        0                     158
    ## 164                   0        2                     122
    ## 165                   1        0                     175
    ## 166                   0        0                     168
    ## 168                   1        2                     159
    ## 169                   0        2                     156
    ## 170                   0        0                     138
    ## 171                   0        0                     112
    ## 172                   0        2                     111
    ## 173                   0        0                     143
    ## 174                   0        2                     157
    ## 175                   0        2                     132
    ## 176                   0        0                      88
    ## 177                   1        0                     147
    ## 178                   0        2                     105
    ## 179                   0        0                     162
    ## 180                   1        2                     173
    ## 181                   0        2                     166
    ## 182                   0        2                     150
    ## 183                   0        2                     178
    ## 184                   0        2                     145
    ## 185                   0        2                     161
    ## 186                   0        0                     179
    ## 187                   1        0                     194
    ## 188                   0        0                     120
    ## 189                   0        2                     195
    ## 190                   0        2                     146
    ## 191                   0        0                     163
    ## 192                   0        0                     122
    ## 194                   1        0                     106
    ## 195                   0        2                     115
    ## 196                   0        2                     125
    ## 197                   1        2                     131
    ## 198                   0        2                     152
    ## 199                   0        0                     162
    ## 200                   0        2                     125
    ## 201                   0        2                     159
    ## 202                   0        0                     154
    ## 203                   1        0                     173
    ## 204                   0        0                     133
    ## 205                   0        0                     161
    ## 206                   0        2                     147
    ## 207                   0        2                     130
    ## 208                   0        2                     126
    ## 209                   0        0                     155
    ## 210                   0        0                     154
    ## 211                   0        0                     170
    ## 212                   0        0                     182
    ## 213                   0        2                     168
    ## 214                   1        0                     165
    ## 215                   0        0                     160
    ## 216                   0        2                     162
    ## 217                   0        0                     172
    ## 218                   0        2                     152
    ## 219                   0        0                     122
    ## 220                   0        2                     182
    ## 221                   0        2                     172
    ## 222                   0        2                     167
    ## 223                   0        0                     179
    ## 224                   0        0                      95
    ## 225                   0        0                     169
    ## 226                   0        0                     192
    ## 227                   0        0                     143
    ## 228                   0        0                     172
    ## 229                   0        2                     108
    ## 230                   0        2                     132
    ## 231                   0        2                     169
    ## 232                   0        1                     117
    ## 233                   0        2                     126
    ## 234                   0        2                     121
    ## 235                   0        0                     163
    ## 236                   0        2                     116
    ## 237                   1        2                     103
    ## 238                   0        2                     144
    ## 239                   0        0                     162
    ## 240                   0        0                     162
    ## 241                   0        0                     153
    ## 242                   0        0                     163
    ## 243                   0        0                     163
    ## 244                   0        0                     145
    ## 245                   1        0                      96
    ## 246                   0        0                      71
    ## 247                   0        0                     156
    ## 248                   0        2                     118
    ## 249                   0        0                     168
    ## 250                   1        2                     140
    ## 251                   0        0                     126
    ## 252                   0        0                     105
    ## 253                   0        0                     105
    ## 254                   0        2                     157
    ## 255                   0        0                     181
    ## 256                   0        0                     173
    ## 257                   0        0                     142
    ## 258                   0        1                     116
    ## 259                   0        2                     143
    ## 260                   0        0                     141
    ## 261                   0        0                     149
    ## 262                   1        2                     152
    ## 263                   0        0                     171
    ## 264                   0        0                     169
    ## 265                   0        2                     125
    ## 266                   0        0                     125
    ## 268                   1        0                     134
    ## 269                   0        0                     181
    ## 270                   0        0                     150
    ## 271                   0        2                     138
    ## 272                   0        2                     138
    ## 273                   0        0                     120
    ## 274                   0        0                     125
    ## 275                   0        0                     162
    ## 276                   0        2                     155
    ## 277                   0        2                     152
    ## 278                   0        0                     152
    ## 279                   0        2                     164
    ## 280                   0        0                     131
    ## 281                   0        0                     143
    ## 282                   0        0                     179
    ## 283                   0        1                     130
    ## 284                   0        0                     174
    ## 285                   0        0                     161
    ## 286                   0        1                     140
    ## 287                   1        2                     146
    ## 289                   0        2                     163
    ## 290                   0        0                     169
    ## 291                   0        2                     150
    ## 292                   0        0                     166
    ## 293                   0        0                     144
    ## 294                   0        2                     144
    ## 295                   0        0                     136
    ## 296                   0        0                     182
    ## 297                   1        2                      90
    ## 298                   0        0                     123
    ## 299                   0        0                     132
    ## 300                   1        0                     141
    ## 301                   0        0                     115
    ## 302                   0        2                     174
    ##     exercise_induced_angina st_depression st_slope num_major_vessels
    ## 1                         0           2.3        3                 0
    ## 2                         1           1.5        2                 3
    ## 3                         1           2.6        2                 2
    ## 4                         0           3.5        3                 0
    ## 5                         0           1.4        1                 0
    ## 6                         0           0.8        1                 0
    ## 7                         0           3.6        3                 2
    ## 8                         1           0.6        1                 0
    ## 9                         0           1.4        2                 1
    ## 10                        1           3.1        3                 0
    ## 11                        0           0.4        2                 0
    ## 12                        0           1.3        2                 0
    ## 13                        1           0.6        2                 1
    ## 14                        0           0.0        1                 0
    ## 15                        0           0.5        1                 0
    ## 16                        0           1.6        1                 0
    ## 17                        0           1.0        3                 0
    ## 18                        0           1.2        1                 0
    ## 19                        0           0.2        1                 0
    ## 20                        0           0.6        1                 0
    ## 21                        1           1.8        2                 0
    ## 22                        0           1.0        1                 0
    ## 23                        0           1.8        2                 0
    ## 24                        0           3.2        1                 2
    ## 25                        1           2.4        2                 2
    ## 26                        0           1.6        2                 0
    ## 27                        0           0.0        1                 0
    ## 28                        0           2.6        3                 0
    ## 29                        0           1.5        1                 0
    ## 30                        1           2.0        2                 0
    ## 31                        0           1.8        1                 2
    ## 32                        1           1.4        1                 2
    ## 33                        0           0.0        1                 0
    ## 34                        0           0.5        2                 0
    ## 35                        1           0.4        1                 0
    ## 36                        0           0.0        1                 0
    ## 37                        1           2.5        2                 0
    ## 38                        1           0.6        2                 1
    ## 39                        1           1.2        2                 1
    ## 40                        1           1.0        2                 0
    ## 41                        0           1.0        2                 3
    ## 42                        1           1.4        1                 0
    ## 43                        0           0.4        1                 2
    ## 44                        0           1.6        1                 0
    ## 45                        0           0.0        1                 0
    ## 46                        0           2.5        2                 1
    ## 47                        0           0.6        1                 0
    ## 48                        0           2.6        2                 0
    ## 49                        0           0.8        1                 1
    ## 50                        0           1.2        3                 0
    ## 51                        0           0.0        1                 1
    ## 52                        0           0.4        1                 0
    ## 53                        0           0.0        1                 1
    ## 54                        0           0.0        1                 0
    ## 55                        1           1.4        1                 1
    ## 56                        1           2.2        2                 1
    ## 57                        0           0.6        2                 1
    ## 58                        0           0.0        1                 0
    ## 59                        0           0.5        3                 1
    ## 60                        1           1.4        1                 1
    ## 61                        1           1.2        2                 0
    ## 62                        1           1.4        3                 0
    ## 63                        1           2.2        2                 3
    ## 64                        0           0.0        1                 0
    ## 65                        0           1.4        2                 1
    ## 66                        1           2.8        2                 2
    ## 67                        0           3.0        2                 0
    ## 68                        0           1.6        1                 0
    ## 69                        1           3.4        3                 0
    ## 70                        0           3.6        2                 0
    ## 71                        0           0.8        1                 0
    ## 72                        0           0.2        2                 2
    ## 73                        1           1.8        2                 2
    ## 74                        0           0.6        1                 2
    ## 75                        0           0.0        1                 1
    ## 76                        0           0.8        1                 0
    ## 77                        1           2.8        2                 1
    ## 78                        0           1.5        1                 1
    ## 79                        0           0.2        2                 0
    ## 80                        1           0.8        1                 0
    ## 81                        1           3.0        2                 0
    ## 82                        0           0.4        2                 0
    ## 83                        0           0.0        1                 0
    ## 84                        1           1.6        2                 0
    ## 85                        0           0.2        1                 0
    ## 86                        0           0.0        1                 0
    ## 87                        0           0.0        1                 0
    ## 89                        0           0.0        1                 0
    ## 90                        0           0.5        1                 0
    ## 91                        0           0.4        2                 0
    ## 92                        0           6.2        3                 3
    ## 93                        0           1.8        2                 3
    ## 94                        0           0.6        2                 0
    ## 95                        0           0.0        1                 0
    ## 96                        1           0.0        1                 1
    ## 97                        1           1.2        2                 1
    ## 98                        0           2.6        2                 2
    ## 99                        0           0.8        1                 1
    ## 100                       0           0.0        1                 0
    ## 101                       0           0.0        1                 0
    ## 102                       0           0.0        1                 0
    ## 103                       0           0.0        1                 1
    ## 104                       0           0.0        1                 1
    ## 105                       0           2.0        2                 3
    ## 106                       0           0.0        1                 0
    ## 107                       1           0.0        1                 1
    ## 108                       0           0.4        2                 1
    ## 109                       1           3.6        2                 1
    ## 110                       0           1.2        2                 0
    ## 111                       1           1.0        2                 0
    ## 112                       1           1.2        2                 1
    ## 113                       0           0.0        2                 0
    ## 114                       1           3.0        2                 0
    ## 115                       0           1.2        2                 1
    ## 116                       0           0.0        2                 0
    ## 117                       0           0.0        1                 0
    ## 118                       0           1.4        1                 0
    ## 119                       1           1.8        1                 3
    ## 120                       0           2.8        2                 1
    ## 121                       1           0.0        1                 2
    ## 122                       0           4.0        2                 3
    ## 123                       1           1.2        2                 0
    ## 124                       1           5.6        3                 0
    ## 125                       0           1.4        2                 1
    ## 126                       0           0.6        2                 0
    ## 127                       1           4.0        3                 2
    ## 128                       1           2.8        2                 1
    ## 129                       0           0.0        1                 0
    ## 130                       0           0.0        1                 0
    ## 131                       0           0.4        2                 0
    ## 132                       1           0.0        1                 1
    ## 133                       0           0.0        1                 0
    ## 134                       1           0.0        1                 0
    ## 135                       0           0.2        2                 0
    ## 136                       0           1.4        2                 0
    ## 137                       1           2.6        3                 0
    ## 138                       0           1.4        2                 1
    ## 139                       1           1.6        2                 0
    ## 140                       0           2.4        2                 0
    ## 141                       1           0.0        1                 0
    ## 142                       0           0.2        2                 0
    ## 143                       0           0.0        1                 0
    ## 144                       1           1.8        2                 0
    ## 145                       1           0.6        2                 0
    ## 146                       0           0.0        1                 0
    ## 147                       0           1.0        2                 3
    ## 148                       0           0.0        1                 0
    ## 149                       0           0.0        1                 0
    ## 150                       0           0.0        1                 1
    ## 151                       0           1.2        2                 0
    ## 152                       0           0.6        2                 0
    ## 153                       0           1.6        2                 0
    ## 154                       1           0.8        2                 1
    ## 155                       1           2.2        3                 1
    ## 156                       0           2.4        2                 3
    ## 157                       1           1.6        1                 0
    ## 158                       0           0.0        1                 2
    ## 159                       0           1.2        2                 2
    ## 160                       0           1.0        1                 1
    ## 161                       0           0.0        1                 0
    ## 162                       1           0.0        1                 3
    ## 163                       0           1.6        2                 0
    ## 164                       0           1.0        2                 0
    ## 165                       0           0.0        1                 2
    ## 166                       1           0.0        1                 0
    ## 168                       1           0.0        1                 1
    ## 169                       1           0.0        1                 0
    ## 170                       0           0.0        2                 0
    ## 171                       1           2.9        2                 1
    ## 172                       1           0.0        1                 0
    ## 173                       1           0.0        2                 0
    ## 174                       0           1.2        2                 0
    ## 175                       0           2.0        2                 2
    ## 176                       1           1.2        2                 1
    ## 177                       0           0.1        1                 3
    ## 178                       1           2.1        2                 1
    ## 179                       0           1.9        1                 1
    ## 180                       0           0.0        1                 3
    ## 181                       0           0.5        2                 0
    ## 182                       1           1.9        2                 2
    ## 183                       0           0.8        1                 2
    ## 184                       0           4.2        3                 0
    ## 185                       0           0.0        1                 0
    ## 186                       0           0.0        1                 2
    ## 187                       0           0.8        3                 0
    ## 188                       1           0.0        2                 3
    ## 189                       0           0.0        1                 1
    ## 190                       0           2.0        2                 3
    ## 191                       0           0.0        1                 0
    ## 192                       1           4.2        2                 3
    ## 194                       0           1.9        2                 3
    ## 195                       0           1.5        2                 0
    ## 196                       1           0.9        2                 2
    ## 197                       0           0.1        2                 1
    ## 198                       1           0.2        2                 0
    ## 199                       0           1.1        1                 0
    ## 200                       0           0.0        1                 0
    ## 201                       0           0.0        1                 0
    ## 202                       1           0.0        1                 0
    ## 203                       0           0.2        1                 1
    ## 204                       0           0.2        1                 0
    ## 205                       0           0.0        1                 0
    ## 206                       1           0.0        2                 3
    ## 207                       1           3.0        2                 2
    ## 208                       1           0.9        2                 0
    ## 209                       0           0.0        1                 0
    ## 210                       1           1.4        2                 0
    ## 211                       0           0.0        1                 0
    ## 212                       1           3.8        2                 0
    ## 213                       0           2.0        2                 0
    ## 214                       1           1.0        2                 2
    ## 215                       0           0.0        1                 1
    ## 216                       0           1.9        2                 0
    ## 217                       0           0.0        1                 0
    ## 218                       1           0.0        2                 0
    ## 219                       0           2.0        2                 2
    ## 220                       0           0.0        1                 0
    ## 221                       1           0.0        1                 0
    ## 222                       0           0.0        1                 0
    ## 223                       0           0.0        1                 0
    ## 224                       1           2.0        2                 2
    ## 225                       1           1.8        2                 2
    ## 226                       0           0.7        1                 0
    ## 227                       0           0.1        1                 0
    ## 228                       0           0.0        1                 1
    ## 229                       1           0.0        2                 1
    ## 230                       1           0.1        1                 1
    ## 231                       0           0.1        2                 0
    ## 232                       1           3.4        2                 0
    ## 233                       0           0.8        1                 3
    ## 234                       1           0.2        1                 1
    ## 235                       0           0.0        1                 1
    ## 236                       1           3.2        2                 2
    ## 237                       1           1.6        3                 0
    ## 238                       0           0.8        1                 0
    ## 239                       0           0.0        2                 0
    ## 240                       0           0.0        1                 0
    ## 241                       0           0.0        1                 0
    ## 242                       0           0.0        1                 0
    ## 243                       0           0.0        1                 0
    ## 244                       0           2.6        2                 2
    ## 245                       0           0.0        1                 0
    ## 246                       0           1.0        2                 0
    ## 247                       0           0.1        1                 1
    ## 248                       1           1.0        2                 1
    ## 249                       0           1.0        1                 2
    ## 250                       0           0.0        1                 0
    ## 251                       1           1.5        2                 0
    ## 252                       0           2.0        2                 1
    ## 253                       1           0.2        2                 1
    ## 254                       0           0.6        1                 0
    ## 255                       0           1.2        2                 0
    ## 256                       0           0.0        2                 0
    ## 257                       0           0.3        1                 2
    ## 258                       0           1.1        2                 0
    ## 259                       0           0.0        1                 0
    ## 260                       0           0.3        1                 0
    ## 261                       0           0.3        2                 1
    ## 262                       0           0.0        1                 2
    ## 263                       0           0.9        1                 0
    ## 264                       0           0.0        1                 0
    ## 265                       1           3.6        2                 1
    ## 266                       1           1.8        2                 0
    ## 268                       0           2.2        2                 1
    ## 269                       0           0.0        1                 0
    ## 270                       0           0.0        1                 0
    ## 271                       1           1.9        1                 1
    ## 272                       0           2.3        1                 0
    ## 273                       1           1.8        2                 2
    ## 274                       0           1.6        2                 0
    ## 275                       0           0.8        1                 2
    ## 276                       0           0.6        2                 0
    ## 277                       0           0.0        2                 1
    ## 278                       0           0.0        2                 0
    ## 279                       0           0.0        1                 1
    ## 280                       0           0.6        2                 0
    ## 281                       1           3.0        2                 1
    ## 282                       0           0.0        1                 0
    ## 283                       1           2.0        2                 1
    ## 284                       0           0.0        1                 0
    ## 285                       0           0.0        1                 1
    ## 286                       0           4.4        3                 3
    ## 287                       1           2.8        2                 2
    ## 289                       0           0.0        1                 0
    ## 290                       0           0.0        3                 0
    ## 291                       0           0.8        2                 0
    ## 292                       0           1.2        1                 0
    ## 293                       1           2.8        3                 0
    ## 294                       1           4.0        1                 2
    ## 295                       1           0.0        2                 0
    ## 296                       0           0.0        1                 0
    ## 297                       0           1.0        2                 2
    ## 298                       1           0.2        2                 0
    ## 299                       0           1.2        2                 0
    ## 300                       0           3.4        2                 2
    ## 301                       1           1.2        2                 1
    ## 302                       0           0.0        2                 1
    ##     thalassemia target
    ## 1             6     No
    ## 2             3    Yes
    ## 3             7    Yes
    ## 4             3     No
    ## 5             3     No
    ## 6             3     No
    ## 7             3    Yes
    ## 8             3     No
    ## 9             7    Yes
    ## 10            7    Yes
    ## 11            6     No
    ## 12            3     No
    ## 13            6    Yes
    ## 14            7     No
    ## 15            7     No
    ## 16            3     No
    ## 17            7    Yes
    ## 18            3     No
    ## 19            3     No
    ## 20            3     No
    ## 21            3     No
    ## 22            3     No
    ## 23            3    Yes
    ## 24            7    Yes
    ## 25            7    Yes
    ## 26            3     No
    ## 27            3     No
    ## 28            3     No
    ## 29            3     No
    ## 30            7    Yes
    ## 31            3     No
    ## 32            7    Yes
    ## 33            3    Yes
    ## 34            7     No
    ## 35            3     No
    ## 36            3     No
    ## 37            7    Yes
    ## 38            6    Yes
    ## 39            7    Yes
    ## 40            3     No
    ## 41            7    Yes
    ## 42            7     No
    ## 43            3     No
    ## 44            3     No
    ## 45            3    Yes
    ## 46            7    Yes
    ## 47            3     No
    ## 48            7    Yes
    ## 49            3     No
    ## 50            3     No
    ## 51            3     No
    ## 52            7     No
    ## 53            3    Yes
    ## 54            3     No
    ## 55            7    Yes
    ## 56            7    Yes
    ## 57            7    Yes
    ## 58            7    Yes
    ## 59            3     No
    ## 60            3     No
    ## 61            7    Yes
    ## 62            3     No
    ## 63            7    Yes
    ## 64            3     No
    ## 65            7    Yes
    ## 66            7    Yes
    ## 67            3    Yes
    ## 68            7     No
    ## 69            7    Yes
    ## 70            3    Yes
    ## 71            3     No
    ## 72            7    Yes
    ## 73            7    Yes
    ## 74            6    Yes
    ## 75            3    Yes
    ## 76            3     No
    ## 77            7    Yes
    ## 78            3     No
    ## 79            3     No
    ## 80            7    Yes
    ## 81            3     No
    ## 82            3     No
    ## 83            3     No
    ## 84            7    Yes
    ## 85            3     No
    ## 86            3     No
    ## 87            3     No
    ## 89            3     No
    ## 90            3     No
    ## 91            3     No
    ## 92            7    Yes
    ## 93            7     No
    ## 94            3     No
    ## 95            3     No
    ## 96            7    Yes
    ## 97            7    Yes
    ## 98            7    Yes
    ## 99            3     No
    ## 100           3     No
    ## 101           3     No
    ## 102           3     No
    ## 103           3     No
    ## 104           3     No
    ## 105           7    Yes
    ## 106           7     No
    ## 107           7    Yes
    ## 108           7    Yes
    ## 109           7    Yes
    ## 110           7    Yes
    ## 111           7    Yes
    ## 112           3    Yes
    ## 113           6     No
    ## 114           7    Yes
    ## 115           7    Yes
    ## 116           6     No
    ## 117           3     No
    ## 118           3     No
    ## 119           7    Yes
    ## 120           7    Yes
    ## 121           7    Yes
    ## 122           7    Yes
    ## 123           3     No
    ## 124           7    Yes
    ## 125           3    Yes
    ## 126           3     No
    ## 127           7    Yes
    ## 128           7    Yes
    ## 129           3     No
    ## 130           3     No
    ## 131           7     No
    ## 132           7     No
    ## 133           3     No
    ## 134           3     No
    ## 135           3     No
    ## 136           3     No
    ## 137           7    Yes
    ## 138           7    Yes
    ## 139           7    Yes
    ## 140           3     No
    ## 141           3     No
    ## 142           7    Yes
    ## 143           3     No
    ## 144           7    Yes
    ## 145           7     No
    ## 146           3    Yes
    ## 147           7    Yes
    ## 148           3     No
    ## 149           3     No
    ## 150           3     No
    ## 151           7     No
    ## 152           3     No
    ## 153           7     No
    ## 154           7    Yes
    ## 155           3    Yes
    ## 156           3    Yes
    ## 157           7    Yes
    ## 158           7    Yes
    ## 159           7    Yes
    ## 160           7     No
    ## 161           7     No
    ## 162           3    Yes
    ## 163           3     No
    ## 164           3     No
    ## 165           3     No
    ## 166           7     No
    ## 168           3     No
    ## 169           7    Yes
    ## 170           3     No
    ## 171           7    Yes
    ## 172           7     No
    ## 173           3    Yes
    ## 174           3     No
    ## 175           6    Yes
    ## 176           7    Yes
    ## 177           7     No
    ## 178           6    Yes
    ## 179           3     No
    ## 180           3     No
    ## 181           7    Yes
    ## 182           7    Yes
    ## 183           3     No
    ## 184           7     No
    ## 185           3    Yes
    ## 186           3     No
    ## 187           7     No
    ## 188           6    Yes
    ## 189           7    Yes
    ## 190           7    Yes
    ## 191           3     No
    ## 192           7    Yes
    ## 194           3    Yes
    ## 195           3     No
    ## 196           3    Yes
    ## 197           3     No
    ## 198           3     No
    ## 199           3     No
    ## 200           3    Yes
    ## 201           3     No
    ## 202           3     No
    ## 203           7     No
    ## 204           7     No
    ## 205           7     No
    ## 206           7    Yes
    ## 207           7    Yes
    ## 208           7    Yes
    ## 209           3     No
    ## 210           3    Yes
    ## 211           3     No
    ## 212           7    Yes
    ## 213           3     No
    ## 214           7    Yes
    ## 215           3    Yes
    ## 216           7     No
    ## 217           3     No
    ## 218           3     No
    ## 219           3     No
    ## 220           3     No
    ## 221           3     No
    ## 222           3     No
    ## 223           3     No
    ## 224           7    Yes
    ## 225           3    Yes
    ## 226           3     No
    ## 227           3     No
    ## 228           3     No
    ## 229           3    Yes
    ## 230           3    Yes
    ## 231           3     No
    ## 232           3    Yes
    ## 233           3    Yes
    ## 234           3     No
    ## 235           3     No
    ## 236           3    Yes
    ## 237           7    Yes
    ## 238           7    Yes
    ## 239           3     No
    ## 240           3     No
    ## 241           3     No
    ## 242           3     No
    ## 243           3     No
    ## 244           3    Yes
    ## 245           3     No
    ## 246           3    Yes
    ## 247           7    Yes
    ## 248           3    Yes
    ## 249           7    Yes
    ## 250           3     No
    ## 251           6     No
    ## 252           7    Yes
    ## 253           7     No
    ## 254           3     No
    ## 255           3     No
    ## 256           3     No
    ## 257           3     No
    ## 258           3     No
    ## 259           3     No
    ## 260           7    Yes
    ## 261           3     No
    ## 262           3    Yes
    ## 263           3     No
    ## 264           3     No
    ## 265           3    Yes
    ## 266           6    Yes
    ## 268           6    Yes
    ## 269           7    Yes
    ## 270           3     No
    ## 271           7    Yes
    ## 272           6     No
    ## 273           7    Yes
    ## 274           3     No
    ## 275           3    Yes
    ## 276           7     No
    ## 277           3     No
    ## 278           3     No
    ## 279           3    Yes
    ## 280           3     No
    ## 281           7    Yes
    ## 282           3     No
    ## 283           7    Yes
    ## 284           3     No
    ## 285           7    Yes
    ## 286           6    Yes
    ## 287           6    Yes
    ## 289           7     No
    ## 290           3     No
    ## 291           7    Yes
    ## 292           3     No
    ## 293           6    Yes
    ## 294           7    Yes
    ## 295           3    Yes
    ## 296           3     No
    ## 297           6    Yes
    ## 298           7    Yes
    ## 299           7    Yes
    ## 300           7    Yes
    ## 301           7    Yes
    ## 302           3    Yes

Producing AIC , BIC and RSS values for various feature subset selection.

``` r
library(leaps)
subsets.out<-regsubsets(target~.,data=heart.data)
sso<-summary(subsets.out)

dim(heart.data)
```

    ## [1] 297  14

``` r
ic<-sso$bic-log(297)*13 + 2*13
round(ic,2)
```

    ## [1] -133.11 -181.83 -215.87 -223.03 -226.31 -227.46 -226.92 -224.28

``` r
my.table<-cbind(sso$outmat,round(sso$rss,2),round(ic,2),round(sso$bic,2))



colnames(my.table)[14:15]<-c("Cp","BIC")
print.table( my.table)
```

    ##          age sex chest_pain_typeatypical angina
    ## 1  ( 1 )                                       
    ## 2  ( 1 )                                       
    ## 3  ( 1 )                                       
    ## 4  ( 1 )                                       
    ## 5  ( 1 )                                       
    ## 6  ( 1 )     *                                 
    ## 7  ( 1 )     *                                 
    ## 8  ( 1 )     *                                 
    ##          chest_pain_typenon-anginal pain chest_pain_typeasymptomatic
    ## 1  ( 1 )                                                            
    ## 2  ( 1 )                                 *                          
    ## 3  ( 1 )                                 *                          
    ## 4  ( 1 )                                 *                          
    ## 5  ( 1 )                                 *                          
    ## 6  ( 1 )                                 *                          
    ## 7  ( 1 )                                 *                          
    ## 8  ( 1 )                                 *                          
    ##          resting_blood_pressure cholesterol fasting_blood_sugar rest_ecg
    ## 1  ( 1 )                                                                
    ## 2  ( 1 )                                                                
    ## 3  ( 1 )                                                                
    ## 4  ( 1 )                                                                
    ## 5  ( 1 )                                                                
    ## 6  ( 1 )                                                                
    ## 7  ( 1 )                                                                
    ## 8  ( 1 )                                                        *       
    ##          max_heart_rate_achieved exercise_induced_angina st_depression
    ## 1  ( 1 )                                                              
    ## 2  ( 1 )                                                              
    ## 3  ( 1 )                                                              
    ## 4  ( 1 )                                                 *            
    ## 5  ( 1 )                         *                                    
    ## 6  ( 1 )                         *                                    
    ## 7  ( 1 ) *                       *                       *            
    ## 8  ( 1 ) *                       *                       *            
    ##          st_slope Cp BIC                      
    ## 1  ( 1 )             *   53.34 -133.11 -85.09 
    ## 2  ( 1 )             *   44.41 -181.83 -133.81
    ## 3  ( 1 )          *  *   38.85 -215.87 -167.85
    ## 4  ( 1 )          *  *   37.2  -223.03 -175.01
    ## 5  ( 1 ) *        *  *   36.09 -226.31 -178.29
    ## 6  ( 1 ) *        *  *   35.27 -227.46 -179.44
    ## 7  ( 1 )          *  *   34.66 -226.92 -178.91
    ## 8  ( 1 )          *  *   34.31 -224.28 -176.26

Producing BIC and CP plots.

``` r
subsets2.out<-regsubsets(target~.,data=heart.data,nbest=3, nvmax = 13)
plot(subsets2.out,scale="Cp")
```

![](Variable_Selection_updated_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
plot(subsets2.out,scale="bic")
```

![](Variable_Selection_updated_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->
