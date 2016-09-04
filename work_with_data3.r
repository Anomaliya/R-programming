# text functions

x <- c("item1", "item2", "item3", "item3", "item2")
length(x)
nchar(x[2]) # counts number of elements in the (x)
nchar(x)


substr(x, start, stop) # separate or replace a part of text vector

x <- "assddffgf"
substr(x, 2, 4)
substr(x, 1, 1) <- "2"


grep(pattern, x, ignore.case=F, fixed=F) # searching exact elements in x
grep("item3", x, fixed=T)

# find exact element in x and replace it with specified text

sub(pattern, replacement, x, ignore.case = F, fixed = F) 
x <- sub("item3", replacement = "item4", x, fixed = T)


# strsplit(x, split, fixed=F)

y <- strsplit("abc", "")

d <- c("w", "e", "s")
dg <- c("s", "F", "HHH")

paste(d, 1:3, sep='')

toupper(d)
tolower(dg)
        










