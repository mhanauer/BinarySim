Original works fine why not simulation?
```{r}
### Just test out
intercept_eff = 2.81
INQ_1_diff_eff = 1.05
INQ_2_diff_eff = .77
ISLES_1_diff_eff = 1.51
ISLES_2_diff_eff = .91
MILQ_diff_eff = 1.15
SIS_2_diff_eff = 2
SIS_1_diff_eff = .63
BID_eff = 1.29
CSE_1_eff = 1.46
CSE_2_eff = .64

y_out = list()
runis_out = list()
test_rep_out = list()
INQ_1_diff_out = list()
INQ_2_diff_out = list()
ISLES_1_diff_out = list()
ISLES_2_diff_out = list()
SIS_1_diff_out = list()
SIS_2_diff_out = list()
MILQ_diff_out = list()
BID_out = list()
CSE_1_out = list()
CSE_2_out = list()
y_prob_out = list()
dat_sim = list()
test_rep = list()
test_rep_sum = list()
n = seq(from = 120, to = 300, by = 20)

for(i in 1:length(n)){

INQ_1_diff_out[[i]] = rnorm(n[[i]],0,1)
INQ_2_diff_out[[i]] = rnorm(n[[i]],0,1)
ISLES_1_diff_out[[i]] = rnorm(n[[i]],0,1)
ISLES_2_diff_out[[i]] = rnorm(n[[i]],0,1)
MILQ_diff_out[[i]] = rnorm(n[[i]],0,1)
SIS_1_diff_out[[i]] = rnorm(n[[i]],0,1)
SIS_2_diff_out[[i]] = rnorm(n[[i]],0,1)
BID_out[[i]] = rnorm(n[[i]],0,1)
CSE_1_out[[i]] = rnorm(n[[i]],0,1)
CSE_2_out[[i]] = rnorm(n[[i]],0,1)

y_out[[i]] = intercept_eff + INQ_1_diff_eff*INQ_1_diff_out[[i]] +INQ_2_diff_eff*INQ_2_diff_out[[i]]+ISLES_1_diff_eff*ISLES_1_diff_out[[i]]+ ISLES_2_diff_eff*ISLES_2_diff_out[[i]]+MILQ_diff_eff*MILQ_diff_out[[i]]+ SIS_1_diff_eff*SIS_1_diff_out[[i]] +SIS_2_diff_eff*SIS_2_diff_out[[i]]+BID_eff*BID_out[[i]]+CSE_1_eff*CSE_1_out[[i]]+CSE_2_eff*CSE_2_out[[i]]

y_prob_out[[i]] = exp(y_out[[i]])/(1+exp(y_out[[i]]))
runis_out[[i]] = runif(length(INQ_1_diff_out[[i]]), 0, 1) # This is the random error part
y_out[[i]] = ifelse(runis_out[[i]] < y_prob_out[[i]], 1, 0)
dat_sim[[i]] = data.frame(y_out = y_out[[i]], INQ_1_diff_out = INQ_1_diff_out[[i]], INQ_2_diff_out = INQ_2_diff_out[[i]], ISLES_1_diff_out = ISLES_1_diff_out[[i]], ISLES_2_diff_out = ISLES_2_diff_out[[i]], MILQ_diff_out = MILQ_diff_out[[i]], SIS_1_diff_out = SIS_1_diff_out[[i]], SIS_2_diff_out = SIS_2_diff_out[[i]], BID_out = BID_out[[i]], CSE_1_out = CSE_1_out[[i]], CSE_2_out = CSE_2_out[[i]])
test_rep[[i]] = glm(y_out ~ ., family = binomial(), data = dat_sim[[i]])
test_rep_sum[[i]] =  summary(test_rep[[i]])
}
#glm(y_out ~ ., family = "binomial", data =dat_sim[[1]])
test_rep_sum[[1]]

dim(dat_sim[[5]])
```
