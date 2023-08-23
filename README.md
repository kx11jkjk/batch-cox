# batch-cox
batch cox regression for gene expression profile or Microbial abundance profile
`easy_input_4cox.csv` is input  
example  
```r
a <- read.csv("easy_input_4cox.csv")
batch_cox(data = a, subname = NULL, save_csv = TRUE)
```
