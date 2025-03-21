--- 
title: "R Core Basics Unleashed: a Step-by-Step Guide to Data Cleaning, Stunning Visuals, and Analytic Insights"
author: "Gillian Debra"
date: "2025-03-21"
site: bookdown::bookdown_site
documentclass: book
bibliography: [book.bib, packages.bib]
# url: your book url like https://bookdown.org/yihui/bookdown
# cover-image: path to the social sharing image like images/cover.jpg
description: |
  This is a minimal example of using the bookdown package to write a book.
  The HTML output format for this example is bookdown::gitbook,
  set in the _output.yml file.
link-citations: yes
github-repo: https://github.com/GillianDebra/R_Core_Basics_Unleashed-Data-Preprocessing_Visualization_and_Analysis_using_R.git
---

# Delighted to have you!<br>Welcome to: {-}
![](index_files/figure-html/unnamed-chunk-1-1.gif)<!-- -->

This guide will tackle **data preprocessing** (the various steps to obtain a *"clean and ready"* dataset using e.g., **dplyr**), **data visualization** (using **ggplot2**), and **data analysis** (**linear regression, analysis of variance, and structural equation modelling**), using **R and RStudio**.

If you already had a go with these activities, you may have noticed that they involve several steps, some of which may have taken you by surprise. You received your data file, you inspect it... only to find that it is far from *usable*. The data should be reordered, variables transformed, certain entries removed. There's a lot to look up or to figure out. After some time, the data file is finally *ready to go*. However, you realize that this is only the beginning, the fun has yet to start. 

With a functional Wi-Fi connection, most of these steps are documented online. Thing is, this abundance of information is scattered far and wide across the internet. Additionally, **you may be unaware of certain possibilities, leaving you without** "*quality of life improving*" tips and tricks. 

Drawing from my own experience in R, I want to help you by providing explanations, demonstrations, and by sharing some tips and tricks. I also want to raise your **awareness** of the many possibilities that R offers. If you are aware that something is possible, you can then explore it further on your own.

Before we begin, I want to briefly note two points. **First**, R allows for different *personal styles*. Different code can lead to the same result, some ways are more intuitive for me while others might be more intuitive for you. **Second**, while this guide is made with **"beginner" R users** in mind, it occasionally contains information for **more advanced users**. Keep in mind that I will explain relatively basic code in the early parts. In later sections, I will progressively reduce explanations of the "basics" to focus on the topic at hand.

## What to expect {-}
This guide will tackle the following aspects.

1. **Loading different types of data files**. We'll start with a brief reminder of R packages and how to set a working directory before demonstrating how to open a variety of files such as **.csv and .sas**.

2. **Sharing and communicating your work with others**. This section will seem to jump ahead slightly, but the sooner you learn how **R projects** notably simplify sharing your data files and R scripts, the better. Additionally, I will provide a **short** introduction to making dynamic (online) reports using **R Markdown** to enhance transparency and reproducibility.

3. **Data preprocessing (dplyr) and descriptive calculations**. Learn perform various computations and to shape and clean your data, using traditional methods and the more modern **dplyr** approach.

4. **Data visualization (ggplot2)**. Learn to plot and show your data - one of the most powerful skills, in my humble opinion. To do so, I will introduce you to a flexible toolbox provided by the **ggplot2 package**, helping you unleash your inner artist. 

5. **Reliability, confirmatory and exploratory factor analysis.** Learn to have an indication of how "closely" questionnaire items relate related to one another (**Cronbach's alpha and McDonald's Omega**). In addition, I'll show you techniques that are used to evaluate whether your data "can be explained" (or deemed to be) by underlying constructs. 

6. **Basic regression analysis (lm, glm, lmer) and model assumptions**. Learn to model straight lines, from the basic linear regression to the mixed-effects (multilevel) linear regression. In addition, we will check **several assumptions** of (linear) regression including the distribution of residuals and homoscedasticity.

7. **ANOVA and MANOVA**. This section focuses on linear regression with categorical variables and comparisons of group means. Learn to create various **contrasts** (beyond treatment and sum contrasts) to test your own questions and hypotheses.

8. **Mediation analysis**. Sometimes we want to know why a relation or effect occurs. In this section you will learn how to fit structural equation models to test mediation and moderated mediation models using **lavaan**. In addition, I'll show you how to visualize the results of a mediation analysis by plotting path diagrams.

9. **Missing data and multiple imputation**. Data can go missing due to a variety of reasons and should not be ignored. In this last part, I provide some approaches on how to deal with missing values and how to inspect *patterns of missingness*. Lastly, I provide a step-by-step guide on multiple imputation using the **mice package**.

<br>
<br>

## Packages and R version {-}
All demonstrations in this guide were done using R *version 4.3.2* and RStudio *version 2023.12.0+369*.
The following packages were used throughout this guide:


|Package      |Version    |
|:------------|:----------|
|car          |3.1-2      |
|corrtable    |0.1.1      |
|dplyr        |1.1.2      |
|e1071        |1.7-13     |
|effectsize   |0.8.6      |
|gganimate    |1.0.8      |
|ggcorrplot   |0.1.4.1    |
|ggimage      |0.3.3      |
|ggplot2      |3.4.4      |
|ggpubr       |0.6.0      |
|haven        |2.5.3      |
|introdataviz |0.0.0.9003 |
|knitr        |1.43       |
|lavaan       |0.6-16     |
|lubridate    |1.9.2      |
|MASS         |7.3-60     |
|mice         |3.16.0     |
|pacman       |0.5.1      |
|psych        |2.3.6      |
|readxl       |1.4.3      |
|rockchalk    |1.8.15.7   |
|semTools     |0.5-6      |
|stringr      |1.5.0      |
|transformr   |0.1.4      |



