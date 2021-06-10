---
title: "BankChurners"
author: "Jarod Wright"
date: "6/9/2021"
output: 
  html_document:
    keep_md: true
---
<style type="text/css">
  body{
  font-size: 18pt;
}
</style>

## Bank Churners

This is an analysis of churning customers. Each observation consists of 18 features and the set of all observations is very unbalanced. 



```r
#dbplyr: for data stored in a relational database. Translates your dplyr code to SQL.(included in tidyverse)
# %>% pipe operator 

set.seed(123)

library(tidyverse)
library(caret)
library(caTools) #subset function
library(corrr)
library(RColorBrewer)
library(smotefamily)
library(plotly)
library(rcompanion)
library(corrplot)
library(randomForest)

coul1 <- brewer.pal(9, "Set3") 
coul2 <- brewer.pal(8, "Set2")


BankChurnersData <- read_csv("BankChurners.csv")
attach(BankChurnersData)
#Exclude last two columns and filter missing data
BankChurnersData <- BankChurnersData %>% select(c(1:21)) %>% arrange(Attrition_Flag)
```
Strength of association is calculated for nominal vs nominal with a bias corrected Cramer's V,  numeric vs numeric with Spearman (default) or Pearson correlation, and nominal vs numeric with ANOVA.


```r
BinaryAssingment <- function(x){
  if(x=="Existing Customer"){
    return(0)
    
  } else {
    return(1)
  }
  
}

# Calculate a pairwise association between all variables in a data-frame. In particular nominal vs nominal with Chi-square, numeric vs numeric with Pearson correlation, and nominal vs numeric with ANOVA.
# Adopted from https://stackoverflow.com/a/52557631/590437
mixed_assoc = function(df, cor_method="spearman", adjust_cramersv_bias=TRUE){
  df_comb = expand.grid(names(df), names(df),  stringsAsFactors = F) %>% set_names("X1", "X2")
  
  is_nominal = function(x) class(x) %in% c("factor", "character")
  # https://community.rstudio.com/t/why-is-purr-is-numeric-deprecated/3559
  # https://github.com/r-lib/rlang/issues/781
  is_numeric <- function(x) { is.integer(x) || is_double(x)}
  
  f = function(xName,yName) {
    x =  pull(df, xName)
    y =  pull(df, yName)
    
    result = if(is_nominal(x) && is_nominal(y)){
      # use bias corrected cramersV as described in https://rdrr.io/cran/rcompanion/man/cramerV.html
      cv = cramerV(as.character(x), as.character(y), bias.correct = adjust_cramersv_bias)
      data.frame(xName, yName, assoc=cv, type="cramersV")
      
    }else if(is_numeric(x) && is_numeric(y)){
      correlation = cor(x, y, method=cor_method, use="complete.obs")
      data.frame(xName, yName, assoc=correlation, type="correlation")
      
    }else if(is_numeric(x) && is_nominal(y)){
      # from https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable/124618#124618
      r_squared = summary(lm(x ~ y))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else if(is_nominal(x) && is_numeric(y)){
      r_squared = summary(lm(y ~x))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else {
      warning(paste("unmatched column type combination: ", class(x), class(y)))
    }
    
    # finally add complete obs number and ratio to table
    result %>% mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
  }
  
  # apply function to each variable combination
  map2_df(df_comb$X1, df_comb$X2, f)
}
#------------------------------------------------------------
```
The correlation of all dependent variables are selected below. The most correlated features are used to generate models.


```r
df.cor <- mixed_assoc(BankChurnersData)
Predictor.Correlation <- df.cor %>% filter(assoc > 0.5 & assoc != 1) # significant correlation between all variables
Sig.Corr <- df.cor %>% filter(x == "Attrition_Flag" | y == "Attrition_Flag") %>% filter(assoc != 1) %>% arrange(x, desc(assoc))# correlation of interest 


print(Sig.Corr[1:6,])
```

```
##                   x                     y     assoc  type complete_obs_pairs
## ...1 Attrition_Flag        Total_Trans_Ct 0.3714027 anova              10127
## ...2 Attrition_Flag   Total_Ct_Chng_Q4_Q1 0.2900540 anova              10127
## ...3 Attrition_Flag   Total_Revolving_Bal 0.2630529 anova              10127
## ...4 Attrition_Flag Contacts_Count_12_mon 0.2044905 anova              10127
## ...5 Attrition_Flag Avg_Utilization_Ratio 0.1784103 anova              10127
## ...6 Attrition_Flag       Total_Trans_Amt 0.1685984 anova              10127
##      complete_obs_ratio
## ...1                  1
## ...2                  1
## ...3                  1
## ...4                  1
## ...5                  1
## ...6                  1
```

```r
#correlation network graph, 
BankChurnersData %>%
  select(Total_Trans_Ct, Total_Ct_Chng_Q4_Q1, Total_Revolving_Bal, Contacts_Count_12_mon, Attrition_Flag) %>%
  mixed_assoc() %>%
  select(x, y, assoc) %>%
  spread(y, assoc) %>%
  column_to_rownames("x") %>%
  as.matrix %>%
  as_cordf %>%
  network_plot(min_cor = 0, colours = c("yellow", "blue", "red")) 
```

![](BankChurners_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
Data <- BankChurnersData %>% select(Attrition_Flag,Total_Trans_Ct,Total_Revolving_Bal,Contacts_Count_12_mon,Avg_Utilization_Ratio,Total_Trans_Amt)
Data$Attrition_Flag <- sapply(Data$Attrition_Flag, BinaryAssingment)

intrain      <- createDataPartition(Data$Attrition_Flag, p = 0.8, list = F)
imbal_train  <- Data[intrain, ]
imbal_test   <- Data[-intrain, ]

Churners <- BankChurnersData  %>% filter(Attrition_Flag == "Attrited Customer") 
Customers <- BankChurnersData %>% filter(Attrition_Flag == "Existing Customer")
```

# Plots 

```r
Attritioncount <- table(BankChurnersData$Attrition_Flag) #unbalanced data
barplot(Attritioncount, col=coul1, main = "Proportion of Attrited to Exsiting")
```

![](BankChurners_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
Attrited.CC <- ggplot(BankChurnersData, aes(x=as.factor(Attrition_Flag), fill=as.factor(Card_Category))) + 
     geom_bar() + labs(x = " Attrition", fill = "Card Category") + scale_fill_brewer(palette = "Set2")
ggplotly(Attrited.CC) %>% config(displayModeBar = FALSE)
```

```{=html}
<div id="htmlwidget-92b8efe163fdea27b776" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-92b8efe163fdea27b776">{"x":{"data":[{"orientation":"v","width":[0.9,0.9],"base":[108,583],"x":[1,2],"y":[1519,7917],"text":["count: 1519<br />as.factor(Attrition_Flag): Attrited Customer<br />as.factor(Card_Category): Blue","count: 7917<br />as.factor(Attrition_Flag): Existing Customer<br />as.factor(Card_Category): Blue"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(102,194,165,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Blue","legendgroup":"Blue","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9],"base":[87,488],"x":[1,2],"y":[21,95],"text":["count:   21<br />as.factor(Attrition_Flag): Attrited Customer<br />as.factor(Card_Category): Gold","count:   95<br />as.factor(Attrition_Flag): Existing Customer<br />as.factor(Card_Category): Gold"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(252,141,98,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Gold","legendgroup":"Gold","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9],"base":[82,473],"x":[1,2],"y":[5,15],"text":["count:    5<br />as.factor(Attrition_Flag): Attrited Customer<br />as.factor(Card_Category): Platinum","count:   15<br />as.factor(Attrition_Flag): Existing Customer<br />as.factor(Card_Category): Platinum"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(141,160,203,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Platinum","legendgroup":"Platinum","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9],"base":[0,0],"x":[1,2],"y":[82,473],"text":["count:   82<br />as.factor(Attrition_Flag): Attrited Customer<br />as.factor(Card_Category): Silver","count:  473<br />as.factor(Attrition_Flag): Existing Customer<br />as.factor(Card_Category): Silver"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(231,138,195,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Silver","legendgroup":"Silver","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":48.9497716894977},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,2.6],"tickmode":"array","ticktext":["Attrited Customer","Existing Customer"],"tickvals":[1,2],"categoryorder":"array","categoryarray":["Attrited Customer","Existing Customer"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":" Attrition","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-425,8925],"tickmode":"array","ticktext":["0","2000","4000","6000","8000"],"tickvals":[0,2000,4000,6000,8000],"categoryorder":"array","categoryarray":["0","2000","4000","6000","8000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"count","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.93503937007874},"annotations":[{"text":"Card Category","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false,"displayModeBar":false},"source":"A","attrs":{"259011e2de25c":{"x":{},"fill":{},"type":"bar"}},"cur_data":"259011e2de25c","visdat":{"259011e2de25c":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

```r
Attrited.Gender <- ggplot(BankChurnersData, aes(x=as.factor(Attrition_Flag), fill=as.factor(Gender))) + 
  geom_bar() + labs(x = " Attrition", fill = "Gender") + scale_fill_brewer(palette = "Set2")
ggplotly(Attrited.Gender) %>% config(displayModeBar = FALSE)
```

```{=html}
<div id="htmlwidget-7f818241993a00f95890" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-7f818241993a00f95890">{"x":{"data":[{"orientation":"v","width":[0.9,0.9],"base":[697,4072],"x":[1,2],"y":[930,4428],"text":["count:  930<br />as.factor(Attrition_Flag): Attrited Customer<br />as.factor(Gender): F","count: 4428<br />as.factor(Attrition_Flag): Existing Customer<br />as.factor(Gender): F"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(102,194,165,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"F","legendgroup":"F","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9],"base":[0,0],"x":[1,2],"y":[697,4072],"text":["count:  697<br />as.factor(Attrition_Flag): Attrited Customer<br />as.factor(Gender): M","count: 4072<br />as.factor(Attrition_Flag): Existing Customer<br />as.factor(Gender): M"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(252,141,98,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"M","legendgroup":"M","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":48.9497716894977},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,2.6],"tickmode":"array","ticktext":["Attrited Customer","Existing Customer"],"tickvals":[1,2],"categoryorder":"array","categoryarray":["Attrited Customer","Existing Customer"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":" Attrition","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-425,8925],"tickmode":"array","ticktext":["0","2000","4000","6000","8000"],"tickvals":[0,2000,4000,6000,8000],"categoryorder":"array","categoryarray":["0","2000","4000","6000","8000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"count","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.93503937007874},"annotations":[{"text":"Gender","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false,"displayModeBar":false},"source":"A","attrs":{"2590156bc8fc9":{"x":{},"fill":{},"type":"bar"}},"cur_data":"2590156bc8fc9","visdat":{"2590156bc8fc9":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

```r
Attrited.Education <- ggplot(BankChurnersData, aes(x=as.factor(Attrition_Flag), fill=as.factor(Education_Level))) + 
  geom_bar() + labs(x = " Attrition", fill = "Education") + scale_fill_brewer(palette = "Set2")
ggplotly(Attrited.Education) %>% config(displayModeBar = FALSE)
```

```{=html}
<div id="htmlwidget-77110634c4d428590c98" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-77110634c4d428590c98">{"x":{"data":[{"orientation":"v","width":[0.9,0.9],"base":[1473,7641],"x":[1,2],"y":[154,859],"text":["count:  154<br />as.factor(Attrition_Flag): Attrited Customer<br />as.factor(Education_Level): College","count:  859<br />as.factor(Attrition_Flag): Existing Customer<br />as.factor(Education_Level): College"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(102,194,165,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"College","legendgroup":"College","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9],"base":[1378,7285],"x":[1,2],"y":[95,356],"text":["count:   95<br />as.factor(Attrition_Flag): Attrited Customer<br />as.factor(Education_Level): Doctorate","count:  356<br />as.factor(Attrition_Flag): Existing Customer<br />as.factor(Education_Level): Doctorate"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(252,141,98,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Doctorate","legendgroup":"Doctorate","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9],"base":[891,4644],"x":[1,2],"y":[487,2641],"text":["count:  487<br />as.factor(Attrition_Flag): Attrited Customer<br />as.factor(Education_Level): Graduate","count: 2641<br />as.factor(Attrition_Flag): Existing Customer<br />as.factor(Education_Level): Graduate"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(141,160,203,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Graduate","legendgroup":"Graduate","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9],"base":[585,2937],"x":[1,2],"y":[306,1707],"text":["count:  306<br />as.factor(Attrition_Flag): Attrited Customer<br />as.factor(Education_Level): High School","count: 1707<br />as.factor(Attrition_Flag): Existing Customer<br />as.factor(Education_Level): High School"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(231,138,195,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"High School","legendgroup":"High School","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9],"base":[493,2513],"x":[1,2],"y":[92,424],"text":["count:   92<br />as.factor(Attrition_Flag): Attrited Customer<br />as.factor(Education_Level): Post-Graduate","count:  424<br />as.factor(Attrition_Flag): Existing Customer<br />as.factor(Education_Level): Post-Graduate"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(166,216,84,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Post-Graduate","legendgroup":"Post-Graduate","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9],"base":[256,1263],"x":[1,2],"y":[237,1250],"text":["count:  237<br />as.factor(Attrition_Flag): Attrited Customer<br />as.factor(Education_Level): Uneducated","count: 1250<br />as.factor(Attrition_Flag): Existing Customer<br />as.factor(Education_Level): Uneducated"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(255,217,47,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Uneducated","legendgroup":"Uneducated","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9],"base":[0,0],"x":[1,2],"y":[256,1263],"text":["count:  256<br />as.factor(Attrition_Flag): Attrited Customer<br />as.factor(Education_Level): Unknown","count: 1263<br />as.factor(Attrition_Flag): Existing Customer<br />as.factor(Education_Level): Unknown"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(229,196,148,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Unknown","legendgroup":"Unknown","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":48.9497716894977},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,2.6],"tickmode":"array","ticktext":["Attrited Customer","Existing Customer"],"tickvals":[1,2],"categoryorder":"array","categoryarray":["Attrited Customer","Existing Customer"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":" Attrition","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-425,8925],"tickmode":"array","ticktext":["0","2000","4000","6000","8000"],"tickvals":[0,2000,4000,6000,8000],"categoryorder":"array","categoryarray":["0","2000","4000","6000","8000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"count","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.93503937007874},"annotations":[{"text":"Education","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false,"displayModeBar":false},"source":"A","attrs":{"2590150403b57":{"x":{},"fill":{},"type":"bar"}},"cur_data":"2590150403b57","visdat":{"2590150403b57":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

```r
Attrited.Marital <- ggplot(BankChurnersData, aes(x=as.factor(Attrition_Flag), fill=as.factor(Marital_Status))) + 
  geom_bar() + labs(x = " Attrition", fill = "Marital Status") + scale_fill_brewer(palette = "Set2")
ggplotly(Attrited.Marital) %>% config(displayModeBar = FALSE)
```

```{=html}
<div id="htmlwidget-a28e71fa0a228c5c5a40" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-a28e71fa0a228c5c5a40">{"x":{"data":[{"orientation":"v","width":[0.9,0.9],"base":[1506,7873],"x":[1,2],"y":[121,627],"text":["count:  121<br />as.factor(Attrition_Flag): Attrited Customer<br />as.factor(Marital_Status): Divorced","count:  627<br />as.factor(Attrition_Flag): Existing Customer<br />as.factor(Marital_Status): Divorced"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(102,194,165,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Divorced","legendgroup":"Divorced","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9],"base":[797,3895],"x":[1,2],"y":[709,3978],"text":["count:  709<br />as.factor(Attrition_Flag): Attrited Customer<br />as.factor(Marital_Status): Married","count: 3978<br />as.factor(Attrition_Flag): Existing Customer<br />as.factor(Marital_Status): Married"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(252,141,98,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Married","legendgroup":"Married","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9],"base":[129,620],"x":[1,2],"y":[668,3275],"text":["count:  668<br />as.factor(Attrition_Flag): Attrited Customer<br />as.factor(Marital_Status): Single","count: 3275<br />as.factor(Attrition_Flag): Existing Customer<br />as.factor(Marital_Status): Single"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(141,160,203,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Single","legendgroup":"Single","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.9,0.9],"base":[0,0],"x":[1,2],"y":[129,620],"text":["count:  129<br />as.factor(Attrition_Flag): Attrited Customer<br />as.factor(Marital_Status): Unknown","count:  620<br />as.factor(Attrition_Flag): Existing Customer<br />as.factor(Marital_Status): Unknown"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(231,138,195,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"Unknown","legendgroup":"Unknown","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":48.9497716894977},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,2.6],"tickmode":"array","ticktext":["Attrited Customer","Existing Customer"],"tickvals":[1,2],"categoryorder":"array","categoryarray":["Attrited Customer","Existing Customer"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":" Attrition","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-425,8925],"tickmode":"array","ticktext":["0","2000","4000","6000","8000"],"tickvals":[0,2000,4000,6000,8000],"categoryorder":"array","categoryarray":["0","2000","4000","6000","8000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"count","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.93503937007874},"annotations":[{"text":"Marital Status","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false,"displayModeBar":false},"source":"A","attrs":{"259013bfd7fbf":{"x":{},"fill":{},"type":"bar"}},"cur_data":"259013bfd7fbf","visdat":{"259013bfd7fbf":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```
Synthetic minority oversampling is utilized to deal with the unbalance of the data.


```r
#SMOTE 
smote_train <- SMOTE(imbal_train[,-1],imbal_train[,1] , K=5)
smote_train <- smote_train$data
smote_train$class <- as.factor(smote_train$class)



#logistic regression 
logit.model <- glm(class~.,data = smote_train, family = binomial)
logit.model_prob <- predict(logit.model, newdata=imbal_test,type = "response")
logit.pred <- as.factor(ifelse(logit.model_prob > 0.5, "Attrited Customer", "Existing Customer"))


#random forest
rf.model <- randomForest(class~., data= smote_train, ntree= 500, importance = TRUE)
rf.model_pred<- predict(rf.model, newdata = imbal_test, type = "response")
rf.model_pred<- factor(rf.model_pred, levels = c(0,1), labels=c("Existing Customer", "Attrited Customer"))


plot(rf.model)
```

![](BankChurners_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
imbal_test$Attrition_Flag <- factor(imbal_test$Attrition_Flag, 
                                    levels = c(0,1), labels = c("Existing Customer", "Attrited Customer"))

confusionMatrix(imbal_test$Attrition_Flag,logit.pred) ## poor performance
```

```
## Confusion Matrix and Statistics
## 
##                    Reference
## Prediction          Attrited Customer Existing Customer
##   Attrited Customer               262                60
##   Existing Customer               338              1365
##                                            
##                Accuracy : 0.8035           
##                  95% CI : (0.7855, 0.8206) 
##     No Information Rate : 0.7037           
##     P-Value [Acc > NIR] : < 2.2e-16        
##                                            
##                   Kappa : 0.4557           
##                                            
##  Mcnemar's Test P-Value : < 2.2e-16        
##                                            
##             Sensitivity : 0.4367           
##             Specificity : 0.9579           
##          Pos Pred Value : 0.8137           
##          Neg Pred Value : 0.8015           
##              Prevalence : 0.2963           
##          Detection Rate : 0.1294           
##    Detection Prevalence : 0.1590           
##       Balanced Accuracy : 0.6973           
##                                            
##        'Positive' Class : Attrited Customer
## 
```

```r
confusionMatrix(imbal_test$Attrition_Flag, rf.model_pred)
```

```
## Confusion Matrix and Statistics
## 
##                    Reference
## Prediction          Existing Customer Attrited Customer
##   Existing Customer              1615                88
##   Attrited Customer                56               266
##                                            
##                Accuracy : 0.9289           
##                  95% CI : (0.9168, 0.9397) 
##     No Information Rate : 0.8252           
##     P-Value [Acc > NIR] : < 2.2e-16        
##                                            
##                   Kappa : 0.7444           
##                                            
##  Mcnemar's Test P-Value : 0.009785         
##                                            
##             Sensitivity : 0.9665           
##             Specificity : 0.7514           
##          Pos Pred Value : 0.9483           
##          Neg Pred Value : 0.8261           
##              Prevalence : 0.8252           
##          Detection Rate : 0.7975           
##    Detection Prevalence : 0.8410           
##       Balanced Accuracy : 0.8589           
##                                            
##        'Positive' Class : Existing Customer
## 
```


