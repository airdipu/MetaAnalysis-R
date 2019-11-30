&gt; install.packages(&quot;meta&quot;)
&gt; library(meta)
&gt; dat&lt;-read.csv(&quot;K:\\Cancer Research\\Meta Anik\\new paper\\Cancer.csv&quot;, header =TRUE)
&gt; str(dat)

###Fitting the fixed effect model
Y &lt;- with(dat, log(Gender*Area*p/(n*N)))
V &lt;- with(dat, 1/Area + 1/p + 1/n + 1/N)
&gt; cbind(Y,V)
&gt; result.or &lt;- rma(yi = Y, vi = V, method = &quot;FE&quot;)
&gt; summary(result.or)

###Confidence Interval

&gt; confint(result.or)

###Fitting Random Effects Model

&gt; re &lt;- rma(yi = Y, vi = V, data=dat)
&gt; summary(re)

###Forest plotting

&gt; forest(re, slab = paste(dat$Author, as.character(dat$Year), sep = &quot;, &quot;))

###Funnel Plotting. A common way to investigate potential publication bias in a meta-analysis is the
funnel plot.
Asymmetrical distribution indicates potential publication bias.

&gt; funnel(re)

###Differences measure

&gt; boxplot(Y ~ Area, data = dat)

###Mesuring tau2 for different method
&gt; estimators &lt;- c(&quot;DL&quot;, &quot;REML&quot;, &quot;HE&quot;, &quot;HS&quot;, &quot;SJ&quot;, &quot;ML&quot;, &quot;EB&quot;)
&gt; taus &lt;- sapply(estimators, function(method){re$tau2})
&gt; plot(y = taus, x = 1:length(taus),type = &quot;h&quot;, pch = 19,axes = FALSE, xlab = &quot;Estimators&quot;)
&gt; axis(2, las = 1)
&gt; axis(1, at = 1:length(taus), lab = estimators)
