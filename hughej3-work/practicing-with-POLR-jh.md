---
title: "Practicing with POLR"
author: "Jackson Hughes"
date: "9/21/2021"
output: 
  html_document:
    keep_md: yes
---



### Import data


```r
setwd("G:/Shared drives/CEG Two-Stage Exams Analysis/Jackson/beyond-linear-regression/BeyondLinearRegression/Proportional Odds Logistic Regression")
polrData <- read_csv("polrData.csv")
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   Gender = col_character(),
##   Ethnicity = col_character(),
##   FirstGeneration = col_double(),
##   CourseGrade = col_double(),
##   ActivityType = col_character(),
##   Dominator = col_double()
## )
```

### View data


```r
tibble(polrData)
```

```
## # A tibble: 436 x 6
##    Gender Ethnicity      FirstGeneration CourseGrade ActivityType Dominator
##    <chr>  <chr>                    <dbl>       <dbl> <chr>            <dbl>
##  1 M      Asian-American               1         2.6 Interactive          2
##  2 M      Asian-American               0         3.3 Interactive          6
##  3 M      Asian-American               1         3.4 Constructive         6
##  4 M      Asian-American               0         3   Interactive          2
##  5 M      Asian-American               1         2.4 Interactive          3
##  6 M      Asian-American               0         2.4 Interactive          6
##  7 F      Asian-American               1         1.5 Interactive          5
##  8 F      Asian-American               0         3.4 Constructive         5
##  9 F      Asian-American               1         3.2 Constructive         5
## 10 F      Asian-American               0         3.2 Interactive          2
## # ... with 426 more rows
```

### Visualize data


```r
hist(polrData$Dominator)
```

![](practicing-with-POLR-jh_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
summary(polrData$Dominator)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   3.142   4.000   6.000
```

### Assign `Dominator` as factor; relevel `Ethnicity` so that the reference is white students


```r
polrData$Dominator <- factor(polrData$Dominator)
polrData$Ethnicity <- relevel(as.factor(polrData$Ethnicity), ref="White")
```

### Model selection: start with most complex model


```r
mod1 <- polr(Dominator ~ Gender + FirstGeneration + CourseGrade + Ethnicity +
             ActivityType +  
             Gender*ActivityType + FirstGeneration*ActivityType + Ethnicity*ActivityType, 
             data=polrData, Hess=T)
summary(mod1)
```

```
## Call:
## polr(formula = Dominator ~ Gender + FirstGeneration + CourseGrade + 
##     Ethnicity + ActivityType + Gender * ActivityType + FirstGeneration * 
##     ActivityType + Ethnicity * ActivityType, data = polrData, 
##     Hess = T)
## 
## Coefficients:
##                                                     Value Std. Error  t value
## GenderM                                         -0.369661     0.2537 -1.45706
## FirstGeneration                                  0.009272     0.2440  0.03800
## CourseGrade                                     -0.270974     0.1353 -2.00249
## EthnicityAsian-American                          0.348262     0.2645  1.31684
## EthnicityInternational                           1.397634     0.5154  2.71162
## EthnicityURM                                     0.239261     0.4244  0.56375
## ActivityTypeInteractive                         -0.915170     0.3315 -2.76040
## GenderM:ActivityTypeInteractive                  0.629048     0.3635  1.73063
## FirstGeneration:ActivityTypeInteractive         -0.030446     0.3511 -0.08672
## EthnicityAsian-American:ActivityTypeInteractive  0.337983     0.3857  0.87637
## EthnicityInternational:ActivityTypeInteractive  -0.253590     0.6939 -0.36546
## EthnicityURM:ActivityTypeInteractive             0.244393     0.5769  0.42361
## 
## Intercepts:
##     Value   Std. Error t value
## 1|2 -3.6793  0.5105    -7.2075
## 2|3 -1.4673  0.4769    -3.0768
## 3|4 -0.2802  0.4724    -0.5931
## 4|5  0.5997  0.4751     1.2624
## 5|6  1.8002  0.4946     3.6397
## 
## Residual Deviance: 1384.718 
## AIC: 1418.718
```

* According to Elli Theobald's demonstration, we should remove the `First Generation*Activity Type` interaction because it has the t-value that's closest to zero. Where in the model summary do I find the t-value for this interaction?
* At the end of Theobald's model selection process, she ends up with the following best-fit model:


```r
mod6 <- polr(Dominator ~  CourseGrade + Ethnicity +
             ActivityType, 
             data=polrData, Hess=T)
summary(mod6)
```

```
## Call:
## polr(formula = Dominator ~ CourseGrade + Ethnicity + ActivityType, 
##     data = polrData, Hess = T)
## 
## Coefficients:
##                           Value Std. Error t value
## CourseGrade             -0.2732     0.1334  -2.047
## EthnicityAsian-American  0.5160     0.1922   2.684
## EthnicityInternational   1.2096     0.3498   3.458
## EthnicityURM             0.3297     0.2852   1.156
## ActivityTypeInteractive -0.5715     0.1741  -3.282
## 
## Intercepts:
##     Value   Std. Error t value
## 1|2 -3.4909  0.4713    -7.4075
## 2|3 -1.2866  0.4364    -2.9482
## 3|4 -0.1035  0.4320    -0.2395
## 4|5  0.7706  0.4346     1.7730
## 5|6  1.9634  0.4560     4.3054
## 
## Residual Deviance: 1389.082 
## AIC: 1409.082
```

### Testing model assumptions

#### Assumption 1: are the categories ordered?


```r
MultiNomMod <- multinom(Dominator ~ CourseGrade + Ethnicity + 
                        ActivityType,
                        data = polrData)
```

```
## # weights:  42 (30 variable)
## initial  value 781.207129 
## iter  10 value 683.845386
## iter  20 value 679.563743
## iter  30 value 679.499077
## final  value 679.498352 
## converged
```

```r
AIC(MultiNomMod, mod6)
```

```
##             df      AIC
## MultiNomMod 30 1418.997
## mod6        10 1409.082
```

* `mod6` is better fitting than the multinomial model, which indicates that the assumption that categories are ordered is correct.

#### Assumption 2: are the log-odds between each level proportional?


```r
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)),
    'Y>=5' = qlogis(mean(y >= 5)),
    'Y>=6' = qlogis(mean(y >= 6)))
}

(s <- with(polrData, summary(as.numeric(Dominator) ~ CourseGrade + Ethnicity + 
                             ActivityType, fun=sf)))
```

```
## as.numeric(Dominator)     N= 436 
## 
## +------------+--------------+---+----+--------+----------+----------+----------+---------+
## |            |              |  N|Y>=1|    Y>=2|      Y>=3|      Y>=4|      Y>=5|     Y>=6|
## +------------+--------------+---+----+--------+----------+----------+----------+---------+
## | CourseGrade|     [0.8,2.6)|115| Inf|2.593387|0.70621926|-0.4054651|-1.2305398|-2.246496|
## |            |     [2.6,3.1)|104| Inf|2.628801|0.81093022|-0.3101549|-1.1505720|-2.484907|
## |            |     [3.1,3.6)|124| Inf|2.816264|0.03226086|-1.0560527|-1.7730673|-2.816264|
## |            |     [3.6,4.0]| 93| Inf|2.363210|0.41443378|-0.8421828|-1.9095425|-3.401197|
## +------------+--------------+---+----+--------+----------+----------+----------+---------+
## |   Ethnicity|         White|195| Inf|2.559440|0.15415068|-0.9855351|-1.7446920|-3.865979|
## |            |Asian-American|164| Inf|2.632543|0.60299608|-0.3953127|-1.3411739|-2.032295|
## |            | International| 28| Inf|3.295837|1.79175947| 0.2876821|-0.7472144|-2.120264|
## |            |           URM| 49| Inf|2.420368|0.72391884|-0.8183103|-1.4916549|-2.730029|
## +------------+--------------+---+----+--------+----------+----------+----------+---------+
## |ActivityType|  Constructive|219| Inf|2.683953|0.77652879|-0.3600027|-1.2439781|-2.847812|
## |            |   Interactive|217| Inf|2.530716|0.17556477|-0.9619012|-1.7546199|-2.465104|
## +------------+--------------+---+----+--------+----------+----------+----------+---------+
## |     Overall|              |436| Inf|2.605156|0.46702300|-0.6453565|-1.4776686|-2.641517|
## +------------+--------------+---+----+--------+----------+----------+----------+---------+
```

##### Compare the transitions between the values of each outcome


```r
s[, 7] <- s[, 7] - s[, 6]
s[, 6] <- s[, 6] - s[, 5]
s[, 5] <- s[, 5] - s[, 4]
s[, 4] <- s[, 4] - s[, 3] 
s[, 3] <- s[, 3] - s[, 3]
s
```

```
## as.numeric(Dominator)     N= 436 
## 
## +------------+--------------+---+----+----+---------+----------+----------+----------+
## |            |              |  N|Y>=1|Y>=2|     Y>=3|      Y>=4|      Y>=5|      Y>=6|
## +------------+--------------+---+----+----+---------+----------+----------+----------+
## | CourseGrade|     [0.8,2.6)|115| Inf|   0|-1.887168|-1.1116844|-0.8250747|-1.0159558|
## |            |     [2.6,3.1)|104| Inf|   0|-1.817871|-1.1210851|-0.8404171|-1.3343346|
## |            |     [3.1,3.6)|124| Inf|   0|-2.784003|-1.0883135|-0.7170147|-1.0431964|
## |            |     [3.6,4.0]| 93| Inf|   0|-1.948776|-1.2566165|-1.0673597|-1.4916549|
## +------------+--------------+---+----+----+---------+----------+----------+----------+
## |   Ethnicity|         White|195| Inf|   0|-2.405289|-1.1396858|-0.7591568|-2.1212871|
## |            |Asian-American|164| Inf|   0|-2.029547|-0.9983088|-0.9458612|-0.6911208|
## |            | International| 28| Inf|   0|-1.504077|-1.5040774|-1.0348965|-1.3730491|
## |            |           URM| 49| Inf|   0|-1.696449|-1.5422292|-0.6733446|-1.2383742|
## +------------+--------------+---+----+----+---------+----------+----------+----------+
## |ActivityType|  Constructive|219| Inf|   0|-1.907424|-1.1365315|-0.8839754|-1.6038340|
## |            |   Interactive|217| Inf|   0|-2.355151|-1.1374660|-0.7927187|-0.7104841|
## +------------+--------------+---+----+----+---------+----------+----------+----------+
## |     Overall|              |436| Inf|   0|-2.138133|-1.1123795|-0.8323121|-1.1638487|
## +------------+--------------+---+----+----+---------+----------+----------+----------+
```

* According to Theobald, these values look "relatively reasonable", and "the direction is the same for each row," which upholds assumption #2.
* At what point would we consider the values to not be reasonable?

### Interpret the results


```r
summary(mod6)
```

```
## Call:
## polr(formula = Dominator ~ CourseGrade + Ethnicity + ActivityType, 
##     data = polrData, Hess = T)
## 
## Coefficients:
##                           Value Std. Error t value
## CourseGrade             -0.2732     0.1334  -2.047
## EthnicityAsian-American  0.5160     0.1922   2.684
## EthnicityInternational   1.2096     0.3498   3.458
## EthnicityURM             0.3297     0.2852   1.156
## ActivityTypeInteractive -0.5715     0.1741  -3.282
## 
## Intercepts:
##     Value   Std. Error t value
## 1|2 -3.4909  0.4713    -7.4075
## 2|3 -1.2866  0.4364    -2.9482
## 3|4 -0.1035  0.4320    -0.2395
## 4|5  0.7706  0.4346     1.7730
## 5|6  1.9634  0.4560     4.3054
## 
## Residual Deviance: 1389.082 
## AIC: 1409.082
```

* This table is hard to interpret because it's in terms of log odds: the log odds of answering one level higher is -0.5715 that of the log odds on the constructive activity.

#### Convert coefficients to odds by exponentiating


```r
exp(cbind(OR = coef(mod6), confint(mod6)))
```

```
## Waiting for profiling to be done...
```

```
##                                OR     2.5 %    97.5 %
## CourseGrade             0.7609438 0.5851794 0.9877516
## EthnicityAsian-American 1.6753748 1.1503330 2.4451284
## EthnicityInternational  3.3521334 1.6871918 6.6773058
## EthnicityURM            1.3905211 0.7938938 2.4326287
## ActivityTypeInteractive 0.5646588 0.4008112 0.7934916
```

* This data is now in terms of odds rather than log-odds. The `OR` column tells us the odds of answering one higher level in the Likert-scale that someone "dominated" the students' groups based on five different predictor variables.

### Visualize the data

#### Plot of the effect of treatment on reporting a dominator


```r
domEff <- Effect("ActivityType",mod6)
plot(domEff, style="stacked", ylab="Probability of Response", xlab= "Treatment", 
     main="Effect of Treatment on Reporting a Dominator")
```

![](practicing-with-POLR-jh_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

#### Plot of the effect of ethnicity on reporting a dominator


```r
EthnicitydomEff <- Effect("Ethnicity",mod6)
plot(EthnicitydomEff, style="stacked", ylab="Probability of Response", xlab= "Ethnicity", 
     main="Effect of Ethnicity on Reporting a Dominator")
```

![](practicing-with-POLR-jh_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

#### Plot of the effect of course grade on reporting a dominator


```r
GradedomEff <- Effect("CourseGrade",mod6)
plot(GradedomEff, style="stacked", ylab="Probability of Response", xlab= "Course Grade", 
     main="Effect of Course Grade on Reporting a Dominator")
```

![](practicing-with-POLR-jh_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

### Notes/Questions
* I don't know where to find the t-values of the interactions in the summary tables for the POLR models; they don't seem to be labeled.
* I had to make `Ethnicity` a factor rather than a character value in order to relevel it to set the reference group as white students. It seems like I still got valid results... is this okay?
* What does censored regression look like in R?
