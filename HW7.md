#�}���f�w��model

## ��ƫe�B�z

### ���Ū��


```{r message=F,warning=F}
if(!require("mlbench")) {
  install.packages("mlbench", dep=TRUE)
  library("mlbench")
}

data(PimaIndiansDiabetes) 
str(PimaIndiansDiabetes) 
```
�W���O�N���Ū�J�A�@��8�ӰѼƻP1�ӵ��G(pos, neg)
```{r message=F,warning=F}
 'data.frame':	768 obs. of  9 variables:
 $ pregnant: num  6 1 8 1 0 5 3 10 2 8 ...
 $ glucose : num  148 85 183 89 137 116 78 115 197 125 ...
 $ pressure: num  72 66 64 66 40 74 50 0 70 96 ...
 $ triceps : num  35 29 0 23 35 0 32 0 45 0 ...
 $ insulin : num  0 0 0 94 168 0 88 0 543 0 ...
 $ mass    : num  33.6 26.6 23.3 28.1 43.1 25.6 31 35.3 30.5 0 ...
 $ pedigree: num  0.627 0.351 0.672 0.167 2.288 ...
 $ age     : num  50 31 32 21 33 30 26 29 53 54 ...
 $ diabetes: Factor w/ 2 levels "neg","pos": 2 1 2 1 2 1 2 1 2 2 ...
```
### �粒�㪺���
```{r}
PimaIndiansDiabetesC<-
  PimaIndiansDiabetes[complete.cases(PimaIndiansDiabetes),]
c(nrow(PimaIndiansDiabetes),nrow(PimaIndiansDiabetesC))
```
�@768�����
```{r}
[1] 768 768
```
### �N����H�������V�m�ջP���ղ�

�H���N2/3����Ƥ���V�m�ա]Test = F�^�A�ѤU1/3�����ղա]Test = T�^

```{r}
PimaIndiansDiabetesC$Test<-F 
PimaIndiansDiabetesC[sample(1:nrow(PimaIndiansDiabetesC),nrow(PimaIndiansDiabetesC)/3),]$Test<-T 
c(sum(PimaIndiansDiabetesC$Test==F),sum(PimaIndiansDiabetesC$Test==T)) 
```
��Ƶ��Ƥ��O��(�V�m, ����)
```{r message=F,warning=F}
[1] 512 256
```

## �w���ҫ��إ�

### �ҫ��إ�
   
�ѩ�ѼƦh�A�]������ܲ��^�k

```{r warning=F,message=F}
fit<-glm(diabetes~., PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==F,], family="binomial")
if(!require("MASS")) {
  install.packages("MASS", dep=TRUE)
  library("MASS")
}
finalFit<-stepAIC(fit, direction = "both", trace = F)
summary(finalFit)$coefficients
```
```{r warning=F,message=F}
 Estimate   Std. Error    z value     Pr(>|z|)
(Intercept) -8.790367220 0.8689584635 -10.115981 4.692923e-24
pregnant     0.111238301 0.0374095919   2.973524 2.944018e-03
glucose      0.037838437 0.0046249467   8.181378 2.806163e-16
insulin     -0.002504772 0.0009200346  -2.722476 6.479466e-03
mass         0.071177186 0.0163958998   4.341158 1.417340e-05
pedigree     0.768527895 0.3443952802   2.231529 2.564614e-02
age          0.016656436 0.0110652585   1.505291 1.322492e-01
```
### �ҫ�����

�ѤW�z�Ѽƥi���A�H�餺���U���]���O�_�M�}���f�����ͦ������s�A�H�޿�j�k�إ߼ҫ��w���}���f�O�_�����ʡA�g���V�v�B��̨ܳΤƫ�A�ҫ��ϥΰѼƬ�pregnant�Bglucose�Binsulin�Bmass�Bpedigree�Bage�A�@6�ӰѼơA�U�ѼƥN��q�Y�@�ӤH�餺���]���v�T�}���f�����ʪ��{��
 
## �w���ҫ�����

```{r warning=F,message=F,fig.height=4.5}
PosPred<-predict(finalFit,newdata = PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,])
PosAns<-ifelse(PosPred<0.5,"Pos","Neg")
PosAns<-factor(PosAns,levels = c("Pos","Neg"))
if(!require("caret")) {
  install.packages("caret", dep=TRUE)
  library("caret")
}
sensitivity(PosAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
```
```{r warning=F,message=F}
## [1] 0.952381
```
```{r warning=F,message=F}
specificity(PosAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
```
```{r warning=F,message=F}
## [1] 0.962963
```
```{r warning=F,message=F}
posPredValue(PosAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
```
```{r warning=F,message=F}
## [1] 0.9756098
```
```{r warning=F,message=F}
negPredValue(PosAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
```
```{r warning=F,message=F}
## [1] 0.9285714
```
```{r warning=F,message=F}
�H�餺���U�ئ]����}���f�����ʪ��v�T�A�H�޿�j�k�ҫ��w���O�_�y�����ʤ����A�i�o�G

- �ӷP�� `r sensitivity(PosAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$Class)*100`%= 95.2%
- �S���� `r specificity(PosAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$Class)*100`%= 96.2%
- ���ʹw���v `r posPredValue(PosAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$Class)*100`%= 97.5%
- ���ʹw���v `r negPredValue(PosAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$Class)*100`%= 92.8%
```