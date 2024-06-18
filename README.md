
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinyTim

<!-- badges: start -->
<!-- badges: end -->

## The Binomial Distribution

The goal of shinyTim is to allow users to explore the a beta prior for a
binomial probability of success. The binomial distribution is use to
model situations where we have conducted a series of $n$ experiments,
each of which have only two possible outcomes, for example “Heads” and
“Tails, or Yes” and “No”, or “Success” and “Failure,” and recorded the
number of times one of those outcomes has occurred. For example, I might
record the number times I observed “Heads” in $n$ tosses of a coin.
There are four conditions that must be satisfied to use the binomial
distribution. These are

1.  There must be only two possible outcomes in each experiment.
2.  The experiments must be indepedent of each other.
3.  The probability of success, $p$, should constant for every
    experiment.
4.  The number of experiments must be finite and fixed in advance.

If these conditions are met, then the binomial distribution can be used
to compute the probability of observing $x$ successes in $n$ trials.
This is usually written as $$
\Pr(X=x;n,p) = \binom{n}{x}p^x(1-p)^{n-x}.
$$

## Frequentist Estimation Of The Probability Of Success, $p$

It is very common to see a situation where someone has observed $x$
successes in $n$ trials/experiments, and wants to estimate $p$. If the
conditions of the binomial distribution hold, then the *frequentist*
(and the *Maximum Likelihood Estimate*—the MLE) is $$
\hat{p}-\frac{x}{n}.
$$ This estimate is also easy to justify from a logical point of view
rather than appealing directly to Statistics.

## Bayesian Estimation Of The Probability Of Success, $p$

Bayesian estimation, in theory, requires a paradigm. In reality, it
requires you to understand that in the Bayesian framework, we do not
believe there is *one true value* (albeit unknown) of $p$, but rather it
follows a distribution. In order to carry out Bayesian estimation we
need to describe our *belief* about the value of $p$ prior to the
observation of any data. Our belief is usually represented by a
probability distribution. There are infinitely many choices for this
distribution, but it is common to choose the *conjugate* prior for the
binomial distribution—the Beta distribution. Conjugacy simply means that
the posterior distribution—the result of combining the prior with the
likelihood of the parameter given the data—is in the same family
distributions as the prior. In our specific case, if we assume a Beta
prior for $p$, then the posterior distribution of $p$ will also be a
Beta distribution, with the parameters altered by the data. In
statistics we write: $$
f(p|n,x)\propto \cal{L}(p|n, x) \times g(p)
$$ where $f(p|n,x)$ is the posterior distribution of $p$ given the data
($x$ and $n$), $\cal{L}(p|n, x)$ is the *likelihood* of $p$ given the
data, and $g(p)$ the the probability (density) function that represents
our prior belief. If $$
g(p) = \frac{1}{B(\alpha, \beta)}p^{\alpha-1}(1-p)^{\beta-1},
$$ and $$
\cal{L}(p|n, x) = p^x(1-p)^{n-x}
$$ then it can be shown that $$
f(p|n,x) = \frac{1}{B(\alpha + x, \beta + n - x)}p^{\alpha + x -1}(1-p)^{\beta + n - x -1}
$$ which we recognise as the probability density function for a Beta
distribution with parameters $\alpha^\prime = \alpha+x$, and
$\beta^\prime = \beta+n-x$. This is useful because the mean of a Beta
distribution with parameters $\alpha$ and $\beta$ is given by $$
\mathrm{E}[p] = \frac{\alpha}{\alpha+\beta}.
$$ It is not hard to see therefore, that the *posterior mean*, which we
might use as point estimate for $p$, is given by $$
\mathrm{E}[p|n,x] = \frac{\alpha+x}{\alpha+x+\beta+n-x}=\frac{\alpha+x}{\alpha+\beta+n}.
$$ It is also easy to see that this will always be different from the
frequentist estimate as it will be *biased* be the prior. However, it is
also a very helpful estimator, because it deals very nicely with
situations where we a) observe zero successes ($x=0$), and b) we have
knowledge that the true probability is not zero—or more correctly in the
Bayesian context we believe that $\Pr(p > 0) > 0$. Why? Because the
posterior mean is $$
\mathrm{E}[p|n,x=0] = \frac{\alpha}{\alpha+\beta+n},
$$ which is greater than zero for any value of $\alpha > 0$ and this is
a requirement of the Beta distribution (that $\alpha,\beta>0$).

## Bias Is Not As Big A Problem As You Think It Is

It is easy to show that although the Bayesian estimate is biased, but
also that it has a lower mean squared error than the frequentist. What
does that mean? If we regard $p$ as the true value, and $\hat{p}$ as our
estimator of $p$, then Mean Squared Error (MSE) is defined as
$\mathrm{E}[(p-\hat{p})^2]$. It can be shown firstly that $$
\mathrm{MSE}(\hat(p)) = \mathrm{Var}[\hat{p}]+\mathrm{Bias}[p,\hat{p}]^2.
$$ That means that even taking into account bias, the Bayesian estimator
does a better job in terms of being closer to the truth.

## What Does This App Do?

It lets users explore different Beta priors, and see the effect it has
on the posterior.

## What Is Up With The Name?

My friend, Dr Tim Kalafut, who was asking me about this prior is named,
well, Tim. And then I have a *quirky = bad* since of humor, and given we
were talking about tiny prior means, and of course given Charles
Dickens, the pun was obvious (to at least me).
