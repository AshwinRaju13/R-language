---
title: "Partition"
author: "Ashwin"
date: "September 27, 2017"
output: html_document
---
creating partition for the data.
```{r, include= FALSE}
loan_data= read.csv("c:/Users/Akesh/Desktop/tutorial/DATA/train1.csv", header= T)
head(loan_data)
```

```{r}
library("caret")
attach(loan_data)
part= createDataPartition(loan_data$LoanAmount, p = 1/4, list = F)

```
we have created partition for the given data. Now, we have to provide variable names for testing and training the data.
```{r}
part_train= lung_data[-part, ]
part_test= lung_data[part, ]
str(part_test)
str(part_train)
```

```{r}
```

