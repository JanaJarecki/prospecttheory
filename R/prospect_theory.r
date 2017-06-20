#' Prospect theory
#' 
#' 
#' Implementation of prospect theory with parameters from Tversky & Kahneman (1992). 
#' The prospect_theory function calculates the choice probability
#' to select gamble 2 of two gambles.
#' The choice rule is a logit function.
#' 
#' 
#' @param g1 the first gamble in the form of a n x 2 matrix, where column 1 contains outcomes, columns 2 contains probabilities
#' @param g2 the second gamble
#' @return the probability to chose gambe 2

#' @export
prospect_theory <- function(g1, g2)
{
    if (dim(g1)[2] != 2 | dim(g2)[2] != 2)
        stop("gambles must be a n x 2 matrix, where n is the number of outcomes.")
    if (sum(g1[,2]) != 1 | sum(g2[,2]) != 1)
        stop("gambles must contain probabilities that sum up to 1 in the second column.")

    g1pt <- matrix(0, nrow(g1), ncol(g1))
    g2pt <- matrix(0, nrow(g2), ncol(g2))

    g1pt[,1] <- value_fun(g1[,1])
    g2pt[,1] <- value_fun(g2[,1])

    g1pt[,2] <- weighting_fun(g1[,2], x = g1[,1])
    g2pt[,2] <- weighting_fun(g2[,2], x = g2[,1])

    v1 <- g1pt[,1] %*% g1pt[,2]
    v2 <- g2pt[,1] %*% g2pt[,2]

    pg2 <- choice_rule(v1, v2)

    return(pg2)  
}


value_fun <- function(x, type = "power")
{
    # Tversky & Kahneman 1992, p. 309 and 311
    if (x >= 0)
    {
        return(x^0.88)
    }

    if (x < 0)
    {
        return(-2.25 * (-x)^0.88)
    }
}
value_fun <- Vectorize(value_fun, vectorize.args = c("x"))


weighting_fun <- function(x, p, type = "TK")
{
    if (x >= 0)
    {
        pp <- p^0.61
        ppp <- (1-p)^0.61
        return( (pp / (pp+ppp)^(1/0.61)) )
    }
    if (x < 0)
    {
        pp <- p^0.69
        ppp <- (1-p)^0.69
        return( (pp / (pp+ppp)^(1/0.69)) )
    }  
}
weighting_fun <- Vectorize(weighting_fun, vectorize.args = c("x", "p"))


choice_rule <- function(v1, v2, type = "logit")
{
    # Logit choice rule
    return(1 / (1 + exp(v1 - v2)))
}


#######################################################################
## Check prospect theory value functions graphcally

# # library(devtools); install_github("janajarecki/themejj")
# library(ggplot2)
# library(themejj);theme_set(themejj())

# ggplot() +ylim(-4,4) + xlim(-2,2) +stat_function(aes(x = seq(-2,2,.01)), fun = value_fun) +ggtitle("Value Function") +xlab("Value") +ylab("Subjective Value") +theme(panel.grid.major = element_line(color="grey80"))

# dt <- data.frame(x = seq(0,1,.01))
# ggplot(dt, aes(x)) +ylim(0,1) + xlim(0,1) +stat_function(fun = function(x) x, linetype = 1) +stat_function(fun = weighting_fun, args = list(x = 1), linetype = 2) +stat_function(fun = weighting_fun, args = list(x = -1), linetype = 3) +ggtitle("Weighting Function") +xlab("Probability") +ylab("Subjective probability") +theme(panel.grid.major = element_line(color="grey80"))