an <- function(n){(n^2)}
bn <- function(n){(-1)^(2n)}
cn <- function(n){n^(-n)}
dn <- function(n){sqrt(log(n))}
en <- function(n){n^(exp(1) - pi)}
fn <- function(n){(pi-exp(1))^n}
gn <- function(n){(-4)^n}
hn <- function(n){sin(-n)}

bn_an <- function(n){bn(n)/an(n)}

x <- 1:100
plot(x, fn(x)/en(x), type ="l", col = "dark red")
lines(x, gn(x))


gn(x)

fit<-sca(ple4, ple4.index)
a4a.ple4 <- ple4 + fit
wireframe(data ~ age + year, data = as.data.frame(harvest(a4a.ple4)), drape = TRUE, main="Fishing mortality", screen = list(x = -90, y=-45))

library(plotly)
library(reshape2)

tmp = as.data.frame(harvest(a4a.ple4))
tmp = tmp[,c(1,2,7)]
df <- acast(tmp, age~year, value.var="data")
fig <- plot_ly(z = ~ df)
fig <- fig %>% add_surface()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'year'),
                    yaxis = list(title = 'age'),
                    zaxis = list(title = 'f')))

fig

#### Catchability
# year fraction before the survey
fit
sfrac <- mean(range(ple4.index[1])[c("startf", "endf")])
Z <- (m(ple4) + harvest(fit))*sfrac # check M * sfrac
lst <- dimnames(fit@index[[1]])
lst$x <- stock.n(fit)*exp(-Z)
stkn <- do.call("trim", lst)
tmp = as.data.frame(index(fit)[[1]]/stkn)
tmp = tmp[,c(1,2,7)]
df <- acast(tmp, age~year, value.var="data")

fig <- plot_ly(z = ~ df)
fig <- fig %>% add_surface()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'year'),
                                   yaxis = list(title = 'age'),
                                   zaxis = list(title = 'Catchability')))

fig

