    library(ggplot2)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(car) # For Levene's test and Anova()

    ## Loading required package: carData

    ## 
    ## Attaching package: 'car'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

    # Load the Data
    df <- read.csv("Alzheimers Mice Data.csv")

    df$AD_Status <- as.factor(df$AD_Status)
    df$Treatment <- as.factor(df$Treatment)

    # Check the structure to ensure conversion worked
    str(df)

    ## 'data.frame':    40 obs. of  4 variables:
    ##  $ AD_Status: Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Treatment: Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 2 2 2 2 2 ...
    ##  $ Training : int  12 15 13 12 14 15 17 16 17 14 ...
    ##  $ Memory   : int  10 12 13 10 13 13 13 14 15 11 ...

    # Descriptive Statistics
    summary_stats <- df %>%
      group_by(AD_Status, Treatment) %>%
    summarise(
      Mean_Memory = mean(Memory),
      SD_Memory = sd(Memory),
      Mean_Training = mean(Training),
      Count = n(),
      .groups = "drop"
    )
    print(summary_stats)

    ## # A tibble: 8 × 6
    ##   AD_Status Treatment Mean_Memory SD_Memory Mean_Training Count
    ##   <fct>     <fct>           <dbl>     <dbl>         <dbl> <int>
    ## 1 1         1                11.6     1.52           13.2     5
    ## 2 1         2                13.2     1.48           15.8     5
    ## 3 1         3                12.4     2.07           15.2     5
    ## 4 1         4                11.2     1.30           13.6     5
    ## 5 2         1                 8.6     0.894          15.4     5
    ## 6 2         2                 7.6     1.95           15.8     5
    ## 7 2         3                 8.2     0.837          15.2     5
    ## 8 2         4                 6.6     2.07           13.6     5

    #  Visualization (Boxplot)
    ggplot(df, aes(x = Treatment, y = Memory, fill = AD_Status)) +
      geom_boxplot() +
      labs(title = "Effect of Treatment and AD Status on Memory",
           x = "Treatment Type",
           y = "Memory Score") +
      theme_minimal()

![](DELA-ROSA,-R-SA2_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    # Homogeneity of Variance (Levene's Test)
    leveneTest(Memory ~ AD_Status * Treatment, data = df)

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##       Df F value Pr(>F)
    ## group  7  0.8275 0.5722
    ##       32

    options(contrasts = c("contr.sum", "contr.poly"))
    anova_model <- aov(Memory ~ AD_Status * Treatment, data = df)
    shapiro.test(residuals(anova_model))

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  residuals(anova_model)
    ## W = 0.96671, p-value = 0.2817

    qqnorm(residuals(anova_model))
    qqline(residuals(anova_model))

![](DELA-ROSA,-R-SA2_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    Anova(anova_model, type = "III")

    ## Anova Table (Type III tests)
    ## 
    ## Response: Memory
    ##                     Sum Sq Df   F value    Pr(>F)    
    ## (Intercept)         3940.2  1 1568.2488 < 2.2e-16 ***
    ## AD_Status            189.2  1   75.3134 6.449e-10 ***
    ## Treatment             14.5  3    1.9204    0.1461    
    ## AD_Status:Treatment    8.7  3    1.1509    0.3436    
    ## Residuals             80.4 32                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    TukeyHSD(anova_model, which = "Treatment")

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Memory ~ AD_Status * Treatment, data = df)
    ## 
    ## $Treatment
    ##     diff       lwr       upr     p adj
    ## 2-1  0.3 -1.620592 2.2205916 0.9740962
    ## 3-1  0.2 -1.720592 2.1205916 0.9920100
    ## 4-1 -1.2 -3.120592 0.7205916 0.3439678
    ## 3-2 -0.1 -2.020592 1.8205916 0.9989766
    ## 4-2 -1.5 -3.420592 0.4205916 0.1697415
    ## 4-3 -1.4 -3.320592 0.5205916 0.2185144

A two-way analysis of variance (ANOVA) was conducted to examine the
effects of Alzheimer’s disease status (AD vs. non-AD) and treatment
condition (four levels) on memory performance.

There was a significant main effect of AD status on memory scores, F(1,
32) = 75.31, p &lt; .001, indicating that mice with Alzheimer’s disease
differed significantly in memory performance compared to non-AD mice.

The main effect of treatment was not significant, F(3, 32) = 1.92, p =
.146, suggesting that memory scores did not differ significantly across
the four treatment conditions.

The interaction between AD status and treatment was also not
significant, F(3, 32) = 1.15, p = .344, indicating that the effect of
treatment on memory performance did not depend on Alzheimer’s disease
status.

Because the main effect of treatment was not significant, post hoc Tukey
HSD tests revealed no significant pairwise differences between treatment
groups.
