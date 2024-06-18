
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

## Frequentist Estimation of the Probability of Success, $p$

It is very common to see a situation where someone has observed $x$
successes in $n$ trials/experiments, and wants to estimate $p$. If the
conditions of the binomial distribution hold, then the *frequentist*
(and the *Maximum Likelihood Estimate*—the MLE) is $$
\hat{p}-\frac{x}{n}.
$$ This estimate is also easy to justify from a logical point of view
rather than appealing directly to Statistics.

## Bayesian Estimation of the Probability of Success, $p$

Bayesian estimation, in theory, requires a paradigm. In reality, it
requires you to understand that in the Bayesian framework, we do not
believe there is *one true value* (albeit unknown) of $p$, but rather it
follows a distribution. In order to carry out Bayesian estimation we
need to describe our *belief* about the value of $p$ prior to the
observation of any data. Our belief is usually represented by a
probability distribution.
