#(gather first, then group by summarize by spearman function) (c1 variable name, c 2 variable value, c3 selected variable repeated x times)

a <- mtcars

b <- mtcars %>% gather(everything(), -"mpg", key = variable, value = value) %>% 
  group_by(variable) %>%
  dplyr::summarise(rho = myspearmanrho(mpg, value),
                   pval = myspearmanpval(mpg, value)) %>%
  arrange(desc(abs(rho)))


helpertable <- function(data, var1) {

  
  
 c <- data %>% 
  select_if(~is.numeric(.x)) %>%
  gather(everything(), -(!!var1), key = variable, value = value) %>% 
  group_by(variable) %>%
  dplyr::summarise(rho = myspearmanrho((!!var1), value ),
                   pval = myspearmanpval((!!var1), value)) %>%
  arrange(desc(abs(rho)))
 
 return(c)

}



test <- helpertable(NIMHMerged, sym("PD_sad"))



c$value <- ifelse(grepl('^\\d+$', c$value), c$value, NA)

c$value <- as.numeric(c$value)


cor.test(x=h, y=i, method = 'spearman')

i = c(1,2,3,4,5)
h = as.numeric(h)


#get rid of all non numbers in column

c <- cor.test(x=a$cyl, y=a$disp, method = 'spearman')

myspearmanrho <- function(x, y) {

  a <- cor.test(x, y, method = 'spearman')
  
  return(as.numeric(a[[4]]))
  
  }


myspearmanpval <- function(x, y) {
  
  a <- cor.test(x, y, method = 'spearman')
  
  return(as.numeric(a[[3]]))
}


myspearmanrho(a$cyl, a$mpg) 

myspearmanpval(a$cyl, a$mpg) 






###for subject compare

a <- helpertable2(testdat, "PD_Energy", "PD_sad")


testdat <- NIMHMerged %>% select(PD_ID, "PD_Energy", "PD_sad")


helpertable2 <- function(data, var1, var2) {
  
  
  
  c <- data %>% 
    mutate_at(names(data)[2:ncol(data)], ~as.numeric(.x)) %>%
    group_by(PD_ID) %>%
    dplyr::summarise(spear = list(myspearman((!!sym(var1)), (!!sym(var2))))) %>%
    group_by(PD_ID) %>%
    mutate(rho = round(as.numeric(unlist(spear)[[3]]), 4),
           pval = round(as.numeric(unlist(spear)[[2]]), 4)) %>%
    select(-spear) %>%
    arrange(desc(abs(rho)))
  

  
  
  return(c)
  
}

#rewrite helper 1 with list column


#make the PLOT!!!!


###
myspearman <- function(x, y) {
  
  a <- if (has_error(cor.test(x, y, method = 'spearman')))
    
  {c(NA,NA,NA,NA)}
  
  else {cor.test(x, y, method = 'spearman')}
  
  
  return(a)
  
}







myspearman2 <- function(x) {
  
  list(myspearman(x[1], x[2]))
}
  
  

a <- mytest2(NIMHMerged, "PD_Energy", "PD_sad")


mytest <-  function(b, c) {
  
  a <- data.frame(mean1 = mean(b, na.rm = T), mean2 = mean(c, na.rm = T))
  
  return(a)
  
}

mytest2 <- function(x, var1, var2) {
  
  list(mytest(x[[var1]], x[[var2]]))
}



gather(everything(), -(!!var1), key = variable, value = value) %>% 
  group_by(variable) %>%
  dplyr::summarise(spear = list(myspearman((!!var1), value))) %>%
  group_by(variable) %>%
  mutate(rho = round(as.numeric(unlist(spear)[[3]]), 4),
         pval = round(as.numeric(unlist(spear)[[2]]), 4)) %>%
  select(-spear) %>%
  arrange(desc(abs(rho)))



c <- NIMHMerged %>% 
  mutate_at(names(NIMHMerged)[2:ncol(NIMHMerged)], ~as.numeric(.x)) %>%
  group_by(PD_ID) %>%
  summarise(spear = list(myspearman(.x, (!!sym("PD_Energy")), (!!sym("PD_sad"))))) %>%
  mutate(rho = round(as.numeric(unlist(spear)[[3]]), 4),
         pval = round(as.numeric(unlist(spear)[[2]]), 4)) %>%
  select(-spear) %>%
  arrange(desc(abs(rho)))
