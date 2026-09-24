
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinyTim

<!-- badges: start -->

<!-- badges: end -->

## The Binomial Distribution

The goal of shinyTim is to let users explore a Beta prior for a binomial
probability of success. The binomial distribution is used to model
situations where we have conducted a series of $n$ experiments, each of
which has only two possible outcomes, for example "Heads" and "Tails",
"Yes" and "No", or "Success" and "Failure", and recorded the number of
times one of those outcomes has occurred. For example, I might record
the number of times I observed "Heads" in $n$ tosses of a coin. There
are four conditions that must be satisfied to use the binomial
distribution. These are

1.  There must be only two possible outcomes in each experiment.
2.  The experiments must be independent of each other.
3.  The probability of success, $p$, must be constant for every
    experiment.
4.  The number of experiments, $n$, must be fixed in advance.

If these conditions are met, then the binomial distribution can be used
to compute the probability of observing $x$ successes in $n$ trials.
This is usually written as $$
\Pr(X=x;n,p) = \binom{n}{x}p^x(1-p)^{n-x}.
$$

## Frequentist Estimation Of The Probability Of Success, $p$

It is very common to see a situation where someone has observed $x$
successes in $n$ trials/experiments, and wants to estimate $p$. If the
conditions of the binomial distribution hold, then the *frequentist*
estimate (which is also the *Maximum Likelihood Estimate*, or MLE) is
$$
\hat{p}=\frac{x}{n}.
$$ This estimate is also easy to justify from a logical point of view,
without appealing directly to Statistics: it is simply the proportion of
successes we observed.

## Bayesian Estimation Of The Probability Of Success, $p$

Bayesian estimation, in theory, requires a paradigm shift. In practice,
it requires you to accept that, rather than treating $p$ as a single
fixed (albeit unknown) value, we describe our uncertainty about $p$ with
a probability distribution. In order to carry out Bayesian estimation we
need to describe our *belief* about the value of $p$ prior to observing
any data. This belief is represented by a probability distribution
called the *prior*. There are infinitely many choices for this
distribution, but it is common to choose the *conjugate* prior for the
binomial distribution—the Beta distribution. Conjugacy simply means that
the posterior distribution—the result of combining the prior with the
likelihood of the parameter given the data—is in the same family of
distributions as the prior. In our specific case, if we assume a Beta
prior for $p$, then the posterior distribution of $p$ will also be a
Beta distribution, with the parameters updated by the data. In
statistics we write: $$
f(p|n,x)\propto \mathcal{L}(p|n, x) \times g(p)
$$ where $f(p|n,x)$ is the posterior distribution of $p$ given the data
($x$ and $n$), $\mathcal{L}(p|n, x)$ is the *likelihood* of $p$ given
the data, and $g(p)$ is the probability (density) function that
represents our prior belief. If $$
g(p) = \frac{1}{B(\alpha, \beta)}p^{\alpha-1}(1-p)^{\beta-1},
$$ and $$
\mathcal{L}(p|n, x) \propto p^x(1-p)^{n-x}
$$ then it can be shown that $$
f(p|n,x) = \frac{1}{B(\alpha + x, \beta + n - x)}p^{\alpha + x -1}(1-p)^{\beta + n - x -1}
$$ which we recognise as the probability density function for a Beta
distribution with parameters $\alpha^\prime = \alpha+x$ and
$\beta^\prime = \beta+n-x$. This is useful because the mean of a Beta
distribution with parameters $\alpha$ and $\beta$ is given by $$
\mathrm{E}[p] = \frac{\alpha}{\alpha+\beta}.
$$ It is not hard to see, therefore, that the *posterior mean*, which we
might use as a point estimate for $p$, is given by $$
\mathrm{E}[p|n,x] = \frac{\alpha+x}{\alpha+x+\beta+n-x}=\frac{\alpha+x}{\alpha+\beta+n}.
$$ This will generally differ from the frequentist estimate, because it
is pulled towards the prior mean (the two only agree when $x/n$ happens
to equal $\alpha/(\alpha+\beta)$). In other words, it is *biased* by the
prior. However, it is also a very helpful estimator, because it deals
very nicely with situations where we a) observe zero successes ($x=0$),
and b) are confident that the true probability is not zero. The
frequentist estimate in this case is $\hat{p} = 0/n = 0$, whereas the
posterior mean is $$
\mathrm{E}[p|n,x=0] = \frac{\alpha}{\alpha+\beta+n},
$$ which is greater than zero for any value of $\alpha > 0$, and the
Beta distribution requires that $\alpha,\beta>0$.

## Bias Is Not As Big A Problem As You Think It Is

Although the Bayesian estimate is biased, it can have a lower mean
squared error than the frequentist one. What does that mean? If we
regard $p$ as the true value, and $\hat{p}$ as our estimator of $p$,
then the Mean Squared Error (MSE) is defined as
$\mathrm{E}[(\hat{p}-p)^2]$, and it can be shown that $$
\mathrm{MSE}[\hat{p}] = \mathrm{Var}[\hat{p}]+\mathrm{Bias}[\hat{p}]^2.
$$ The frequentist estimate $\hat{p}=x/n$ is unbiased, so its MSE is
just its variance, $$
\mathrm{MSE}\left[\frac{x}{n}\right] = \frac{p(1-p)}{n}.
$$ The posterior mean, $\tilde{p} = (\alpha+x)/(\alpha+\beta+n)$, has
some bias but a smaller variance, and its MSE is $$
\mathrm{MSE}[\tilde{p}] = \frac{np(1-p) + \left(\alpha - (\alpha+\beta)p\right)^2}{(\alpha+\beta+n)^2}.
$$ When the true value of $p$ is reasonably close to the prior mean, the
reduction in variance more than makes up for the bias, and the Bayesian
estimator is, on average, closer to the truth. This is often referred to
as the bias–variance trade-off. It is not a free lunch, though: if the
prior is strong (large $\alpha+\beta$) and centred far from the true
value, the bias dominates and the frequentist estimate does better. As
$n$ grows, the data swamp the prior and the two estimates converge.

Note that this is a *pre-data* comparison: before any data are observed,
we are comparing two estimation procedures, so averaging over the
sampling distribution of $x$ is legitimate even from a Bayesian point of
view. The argument follows Bolstad and Curran (2017, Section 9.3,
p. 174).

The details matter for the situations this app was built for. With a
uniform prior, Beta(1, 1), the posterior mean is pulled towards 1/2, and
the frequentist estimate actually has the lower MSE when $p$ is close to
0 or 1. The two MSEs cross at about $p = 0.14$ (and $0.86$) when
$n = 10$, and as $n$ grows the crossover approaches
$(1 - 1/\sqrt{2})/2 \approx 0.146$, the smaller root of
$(1-2p)^2 = 4p(1-p)$. So the uniform-prior posterior mean has the lower
MSE over roughly the middle 71% of the range, whatever the sample size,
and the higher MSE in the tails. For tiny probabilities, the advantage
of the posterior mean comes from using an informative prior that is
roughly centred on the plausible values of $p$. This is why the app
lets you build the prior from expert judgement, rather than defaulting
to a uniform prior.

## Beta Isn't The Only Prior

In forensic work, the prior usually comes from an expert, who might say
something like: "a priori, I think this might happen about 1 time in
10,000, but I'm willing to let it be as large as 1 time in 100, with
probability 0.95." That is two statements: a typical value, and an upper
value with a probability attached. Two statements are exactly enough to
pin down a prior from a two-parameter family.

The typical value should be treated as the prior *median* ("it is as
likely to be rarer than 1 in 10,000 as it is to be more common"), not
the prior mean. In this region a mean is not just awkward, it is often
impossible. For any distribution on $[0, 1]$ with mean $\mu$, Markov's
inequality gives $\Pr(p \geq t) \leq \mu/t$, so the value that $p$ stays
below with probability $\gamma$ can be at most $\mu/(1-\gamma)$. With a
mean of 1 in 10,000, no distribution at all can have its 0.95 quantile
above $20 \times 10^{-4} = 0.002$, and the best a Beta distribution can
manage is about 0.00058. The expert's statement above cannot be
represented with a mean of 1 in 10,000, whatever family we choose.

With the median, the statement is easy to represent, and the app does
so with two families:

-   **Beta.** The app finds the Beta($\alpha$, $\beta$) with the given
    median and upper quantile numerically. For the statement above this
    is roughly Beta(0.16, 87). It keeps the arithmetic conjugate.
-   **Logit-normal.** Here $\mathrm{logit}(p) = \log\{p/(1-p)\}$ has a
    normal distribution with mean $\mu$ and standard deviation $\sigma$.
    Because quantiles pass straight through the logit, the expert's two
    statements give the parameters directly: $$
    \mu = \mathrm{logit}(\text{median}), \qquad
    \sigma = \frac{\mathrm{logit}(\text{upper value}) - \mu}{\Phi^{-1}(\gamma)},
    $$ where $\gamma$ is the probability attached to the upper value. For
    small $p$ the logit is almost the same as $\log p$, so this is
    effectively a log-normal prior, which matches the way experts tend to
    reason in orders of magnitude ("somewhere between 1 in a million and
    1 in 100"). It is not conjugate, so the app computes the posterior
    by numerical integration on a grid.

Both priors match everything the expert said, but they are not the same
distribution, and they imply different things about the values the
expert did not mention. For the example above, the Beta prior puts the
probability of a success on the next trial at about 1 in 540, and the
logit-normal at about 1 in 300. After a study that observed 0 successes
in 10 trials, the two give about 1 in 610 and 1 in 620. Note that none of
these is 1 in 10,000: the probability for the next trial is the mean,
and an expert who allows for values as large as 1 in 100 has, whether
they realise it or not, pulled the mean well above their typical value.

The part of the prior that matters is the upper tail. The two families
differ enormously in the lower tail, but that has almost no effect on
the probability for the next trial, and in an adversarial setting no one
argues for smaller values anyway. What does matter is the expert's upper
value and the probability attached to it. Using the same example (0
successes in 10 trials), the probability for the next trial changes as
follows when one input is varied and the others are held fixed:

| Input varied | Beta | Logit-normal |
|---|---|---|
| Median from 1 in a million to 1 in 1,000 | 0.0014 to 0.0024 | 0.0012 to 0.0024 |
| Upper value from 1 in 1,000 to 1 in 10 | 0.00025 to 0.0062 | 0.00026 to 0.0035 |
| Probability for the upper value from 0.90 to 0.99 | 0.0027 to 0.00088 | 0.0027 to 0.00061 |

A thousand-fold change in the median barely doubles the answer, while
the upper value and its probability drive almost everything. The
**Sensitivity** tab plots this relationship for the current inputs.
The two families agree closely when the upper value is modest, and
separate when it is large, which is where the choice of family itself
becomes part of the argument.

## What Does This App Do?

It lets users explore priors for a binomial probability, and see their
effect on the posterior and on predictions. You can specify the prior in
one of two ways:

-   **from expert judgement**, by giving a typical value (the median)
    and an upper value, both as "1 in $N$", together with the
    probability that the true value lies below the upper value. The app
    fits both a Beta and a logit-normal prior to these statements, shows
    the fitted parameters, and flags it if the Beta cannot match them
    exactly; or
-   **as a Beta distribution**, by entering $\alpha$ and $\beta$
    directly (for example, $\alpha = \beta = 1$ for the uniform prior).

You then enter the data (the number of successes, $x$, in $n$ trials).
The **Prior and posterior** tab plots the prior and posterior densities
on a log scale, and tabulates, for each prior family, the median, the
upper quantile, the 99th percentile, the chance that $p$ exceeds a
threshold of your choosing, and the mean, all shown both as
probabilities and as "1 in $N$". Setting $n = 0$ shows the prior on its
own.

The **Predictive** tab is built around the quantity we actually need:
the probability that the *next* trial is a success. In a forensic
Bayesian network, for example, a node might need the probability that
foreign DNA is found under a person's fingernails after they held hands
with someone. That is a probability for a single future event, which is
the posterior predictive probability $$
\Pr(\text{success on next trial}|n,x) = \int_0^1 p\,f(p|n,x)\,dp = \frac{\alpha^\prime}{\alpha^\prime+\beta^\prime}.
$$ for a Beta prior (for the logit-normal prior the integral is evaluated
numerically). This happens to equal the posterior mean, because the
probability of a success given $p$ is $p$ itself, so averaging it over
the posterior gives $\mathrm{E}[p|n,x]$.

The distinction matters because of how this is often done in practice.
It is common to treat $(x+1)/(n+2)$ as a smoothed version of $x/n$ (in
the spirit of Laplace smoothing), that is, as the posterior mean used as
a plug-in *estimate* of $p$. That view invites the question of why the
mean, rather than the median or mode, and it suggests that any function
of $p$ can be handled by substituting the estimate. The predictive view
has neither problem: there is no estimator to choose, and the
equality with the posterior mean is a property of a single trial, not a
general rule. It fails as soon as the quantity of interest is not linear
in $p$. Suppose we observed 0 successes in 10 trials and used a uniform
prior, so the posterior is Beta(1, 11):

-   The probability of at least one success in the next 5 trials is
    $1 - B(1, 16)/B(1, 11) = 5/16 \approx 0.313$. Plugging the posterior
    mean into $1-(1-p)^5$ gives $1 - (11/12)^5 \approx 0.353$ instead.
-   If the same study informs two events that are conditionally
    independent given $p$ (say, DNA found under the fingernails of each
    hand), the probability that both occur is
    $\mathrm{E}[p^2|n,x] = 2/(12 \times 13) \approx 0.0128$. The
    plug-in value, $(1/12)^2 \approx 0.0069$, is almost half of that.

In both cases the plug-in approach gives the wrong answer, because it
ignores our uncertainty about $p$.

Returning to the single-trial case, suppose a published transfer study
observed foreign DNA under the fingernails in 0 of 10 hand-holding
experiments. We do not believe the true probability is zero, and the
predictive probability reflects that: with a uniform prior it is
$1/12 \approx 0.083$.

That example also shows why the tab includes a prior-sensitivity table.
When $x = 0$ and $n$ is small, the answer depends heavily on the prior:
with the same 0 out of 10, the Jeffreys prior, Beta(1/2, 1/2), gives
$0.5/11 \approx 0.045$. The table shows your prior (or priors) alongside these
defaults, so you can see how much of the answer comes from the data and
how much from the prior.

Be careful with priors that have $\alpha$ well below 1. Such a prior
piles almost all of its probability extremely close to zero and spreads
the rest thinly over larger values. For example, Beta(0.001, 0.999) has
a mean of 0.001, but puts about 99% of its probability below $10^{-6}$.
After 0 successes in 4 trials, the posterior predictive probability is
0.0002, yet the central 95% credible interval for $p$ is roughly
$(0, 1.3 \times 10^{-12})$, and only about 0.6% of the posterior lies
above its mean. The predictive probability is still correct, but it is
driven almost entirely by the thin tail. The app flags when this
happens. A prior like this says "$p$ is almost certainly negligible, but
might be appreciable", which is rarely what anyone intends.

For more than one future trial, the tab can also show the posterior
predictive distribution of the number of successes, $Y$, in $m$ future
trials. For a Beta prior this is a beta-binomial distribution: $$
\Pr(Y=k|n,x) = \binom{m}{k}\frac{B(\alpha^\prime + k, \beta^\prime + m - k)}{B(\alpha^\prime, \beta^\prime)}, \quad k = 0, 1, \ldots, m.
$$ It is wider than a binomial with the same mean, because it carries
our uncertainty about $p$ as well as the randomness of the future
trials. (For the logit-normal prior the app averages the binomial over
the posterior numerically.) The tab plots this distribution for each
prior family, and tabulates its mean, standard deviation, a 95%
prediction interval, and the probability of at least one success, which
for a Beta prior is $$
\Pr(Y \geq 1|n,x) = 1 - \frac{B(\alpha^\prime, \beta^\prime + m)}{B(\alpha^\prime, \beta^\prime)}.
$$

## What Is Up With The Name?

My friend, Dr Tim Kalafut, who was asking me about this prior, is named,
well, Tim. And then I have a *quirky = bad* sense of humor, and given we
were talking about tiny prior means, and of course given Charles
Dickens, the pun was obvious (to at least me).

## References

Bolstad, W. M. and Curran, J. M. (2017). *Introduction to Bayesian
Statistics* (3rd ed.). Hoboken, NJ: Wiley.

O'Hagan, A., Buck, C. E., Daneshkhah, A., Eiser, J. R., Garthwaite, P.
H., Jenkinson, D. J., Oakley, J. E. and Rakow, T. (2006). *Uncertain
Judgements: Eliciting Experts' Probabilities*. Chichester: Wiley.
