var a = 0 to 10 by 3
for (i <- a zip (a tail)) yield println(i)
for (i <- a) yield println(i)
a tail