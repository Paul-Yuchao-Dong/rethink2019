dd <- data.frame(x=c(3,4),n=c(10,11))
get_binCI <- function(x,n) {
  rbind(setNames(c(binom.test(x,n)$conf.int),c("lwr","upr")))
}
with(dd[1,],get_binCI(x,n))

get_binCI2 <- function(x,n) {
  setNames(c(binom.test(x,n)$conf.int),c("lwr","upr"))
}

x <- (with(dd[1,], get_binCI2(x,n)))
dd %>% 
  do(cbind(.,get_binCI(.$x,.$n)))

library(data.table)
setDT(dd)[,as.list(get_binCI2(x,n)), by = .(x,n)] %>% 
  as.tibble()
