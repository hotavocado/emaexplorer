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
