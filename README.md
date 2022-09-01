Choosing a Husband
================

## The Puzzle

In the court of King Arthur there lived a damsel, Elfreda. She was a
very impressive damsel and was considered the dream wife by every
knight.

Now, it happened that for the next 100 days prospective knights would be
coming to Camelot to be interviewed by Arthur.

Elfreda wanted to choose the best knight to be her husband. She was,
however, in a dilemma. Once the knights had left the castle, they did
not return.

She needed some way to decide, as soon as she saw a knight, whether he
was suitable. Elfreda took her problem to Merlin.

“Let me clarify a few points,” said Merlin. “Am I right in saying that
you have the pick of any knight that comes to the castle, that they
arrive one by one and that you need to make a choice as soon as you see
a knight?”

“Got it in one, Merlin.”

“And, given any two knights, you would be able to say which was the
better?”

“No problem.”

Merlin straightaway realized that this was a probability question and
set to work on the back of a piece of parchment.

A few minutes later he called to Elfreda.

“Cast your eyes over the first 37 suitors and decide which one is the
best. Then, as soon as you see a knight who is better, choose him.”

“Let me get this right. I don’t choose a knight from the first 37, I
just look at them. My choice is made from the remaining 63 knights.”

“That’s it. Good luck and let me know how you get on.”

It was some time before Elfreda returned. One hundred days, in fact.

“Well, how did it go?” said Merlin.

“Not well,” said Elfreda. “The best knight was the first one to arrive,
so I never got to choose one.”

“Oh dear, oh dear. That’s Arthur for you. He always invites the best
knight first. Not random events at all. Certainly not independent
events.

“My maximum likelihood estimation doesn’t work in those circumstances. I
suppose I should have used Markov chains to model the situation. Oh
dear, oh dear….” Merlin went off, muttering.

Why did Merlin instruct Elfreda to ignore the first 37 suitors?

What did Merlin think was the probability that Elfreda would choose the
best knight?

## Solutions

### Analytic

This problem is a new version of what is commonly known as the secretary
problem.

The optimal policy for this problem is a stopping rule. Under it,
Elfreda rejects the first $r − 1$ knights and then selects the first
subsequent knight that is better than the best knight in those first
$r - 1$ knights.

For an arbitrary cutoff r, the probability that the best knight is
selected is:

$$P(r) = \sum_{i=1}^{n} P\left(\text{knight } i \text{ is selected} \cap \text{knight } i \text{ is the best}\right)$$

$$= \sum_{i=1}^{n} P\left(\text{knight } i \text{ is selected} | \text{knight } i \text{ is the best}\right) \cdot P\left(\text{knight } i \text{ is the best}\right)$$

$$= \left[ \sum_{i=1}^{r-1} 0 + \sum_{i=r}^{n} P\left( \left. \begin{array}{l} \text{the best of the first } i - 1 \text{ knights is in the first } r-1 \text{ knights} \end{array} \right|  \text{knight } i \text{ is the best} \right) \right] \cdot \frac{1}{n}$$

$$= \left[\sum_{i=r}^{n} \frac{r-1}{i-1}\right] \cdot \frac{1}{n}$$

$$=\frac{r-1}{n} \sum_{i=r}^{n} \frac{1}{i-1}$$

To identify the optimal cut off we can use the previous formula to
compute the corresponding probability for each possible $r$ cut-off
value and identify the $r$ value producing the maximum probability.

To start we define a function that can calculate the probability of
selecting the best knight given the total sample size $n$ and an
arbitrary cut-off $r$.

``` r
p <- function(r, n) {
  purrr::map2_dbl(r, n, ~ (.x - 1) / .y * sum(1 / (.x:.y - 1)))
}
```

We then calculate the corresponding probability for each possible $r$
cut-off value.

``` r
rs <- 2:100
ps <- p(r = rs, n = 100)
```

And identify the index of the largest probability.

``` r
i <- which.max(ps)
```

We can then use this index to find the cut-off $r$ producing the maximum
probability.

Because $r$ represents the first knight Elfreda might choose, we
subtract 1 to match Merlin’s claim that she not choose among the first
37 suitors.

``` r
rs[i] - 1
```

    ## [1] 37

And we can look up the already computed probability of her successfully
choosing the best knight.

``` r
ps[i]
```

    ## [1] 0.3710428

### Simulation

We might try to double check the analytic solution by seeing if the
results of a simulation are similar.

``` r
n <- 100
r <- 38

mean(
  replicate(
    1000000, {
      knight_scores <- round(rnorm(n), 4)
      max_train <- max(knight_scores[1:(r - 1)])
      x <- knight_scores[r:n] > max_train
      chosen_index <- ifelse(any(x), as.integer(r - 1 + which.max(x)), NA)
      !is.na(chosen_index) && chosen_index == which.max(knight_scores)
    }
  )
)
```

    ## [1] 0.370826
