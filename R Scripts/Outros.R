#Sxx, Sxy

sxx <- sum((x-mean(x))^2)
syy <- sum((y-mean(y))^2)
sxy = syx <- sum((y-mean(y))*(x-mean(x)))

require(magrittr)
%>% #forward operator