# Example: Two-sample t-test power analysis
# Let's say we expect a difference (delta) of 0.5 (effect size), 
# want a power of 0.80, and are using a significance level of 0.05.

#delta 0,5 -> 0,5 punkte unterschied auf der likert
result <- power.t.test(delta = 0.5, sd = 1, sig.level = 0.05, power = 0.80, type = "two.sample", alternative = "two.sided")
print(result)


#delta 1 -> 1 pkt likert unterschied bei t test

result <- power.t.test(delta = 1, sd = 1, sig.level = 0.05, power = 0.80, type = "two.sample", alternative = "two.sided")
print(result)