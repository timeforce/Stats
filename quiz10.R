library(lsr)


#1 Using a t-test, compare verbal scores before and after training in the fixed condition. Is the difference pre-test to post-test significant?
data.f = subset(data, data$cond == "fixed")
data.m = subset(data, data$cond == "malleable")
data.pre = data.frame(data[1:3], data[5], data[7])
data.post = data.frame(data[1:2], data[4], data[6], data[8])
t.test(data.f$verbal.pre, data.f$verbal.post, paired = T)

#2 What are the degrees of freedom for the comparison between pre-test and post-test for the spatial scores?
t.test(data.f$spatial.pre, data.f$spatial.post, paired = T)

#3 Run a Wilcoxon test for the same comparison (pre-test to post-test on spatial scores, fixed condition). Which of the two tests gives the highest p-value for the comparison?
wilcox.test(data.f$spatial.pre, data.f$spatial.post, paired=T)

#4 What is the effect size (Cohen's d) for the difference between pre-test and post-test spatial scores for the fixed condition? (round to two decimal places)
cohensD(data.f$spatial.pre, data.f$spatial.post, method="paired")

#5 Which of the three tasks shows the largest improvements from pre-test to post-test, in the fixed condition?
cohensD(data.f$verbal.pre, data.f$verbal.post, method="paired")
cohensD(data.f$spatial.pre, data.f$spatial.post, method="paired")
cohensD(data.f$intel.pre, data.f$intel.post, method="paired")

#6 Which of the three tasks shows the largest improvements from pre-test to post-test, in the malleable condition?
cohensD(data.m$verbal.pre, data.m$verbal.post, method="paired")
cohensD(data.m$spatial.pre, data.m$spatial.post, method="paired")
cohensD(data.m$intel.pre, data.m$intel.post, method="paired")

#7 Conduct Mann-Whitney comparisons between all tasks at pre-test. Which task(s) differ significantly from the other two in pre-test scores?
wilcox.test(data$spatial.pre, data$verbal.pre, paired=F)
wilcox.test(data$spatial.pre, data$intel.pre, paired=F)
wilcox.test(data$verbal.pre, data$intel.pre, paired=F)

#8 Which feedback condition led to the largest improvements overall?
pre.m = data.m$verbal.pre + data.m$spatial.pre + data.m$intel.pre
post.m = data.m$verbal.post + data.m$spatial.post + data.m$intel.post
cohensD(pre.m, post.m, method="paired")
pre.f = data.f$verbal.pre + data.f$spatial.pre + data.f$intel.pre
post.f = data.f$verbal.post + data.f$spatial.post + data.f$intel.post
cohensD(pre.f, post.f, method="paired")

#9 Which task is largely driving this effect?
cohensD(data.m$verbal.pre, data.m$verbal.post, method="paired")
cohensD(data.m$spatial.pre, data.m$spatial.post, method="paired")
cohensD(data.m$intel.pre, data.m$intel.post, method="paired")
cohensD(data.f$verbal.pre, data.f$verbal.post, method="paired")
cohensD(data.f$spatial.pre, data.f$spatial.post, method="paired")
cohensD(data.f$intel.pre, data.f$intel.post, method="paired")
