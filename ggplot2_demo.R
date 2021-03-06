library(RCurl)
eval(parse(text = getURL("https://raw.github.com/fusion0202/RStat/master/https.csv.R", ssl.verifypeer = FALSE)))
fert <- https.csv("https://raw.github.com/fusion0202/RStat/master/demography/data_files/fert_1947-2010.csv")
library(ggplot2)
g = ggplot(fert, aes(fert$X1, fert$X2))
g = g + geom_point() + geom_line()
g = g + opts(title = "日本の合計特殊出生率（1947～2010）")
g = g + xlab("year") + ylab("fertility rate")
g = g + ylim(c(0, 5))
print(g)

library(RCurl)
eval(parse(text = getURL("https://raw.github.com/fusion0202/RStat/master/ggplot2_demo.R", ssl.verifypeer = FALSE)))

