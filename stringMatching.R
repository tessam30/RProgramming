# - working with text
library(XML)
theURL <- "http://www.loc.gov/rr/print/list/057_chron.html"

presidents <- readHTMLTable(theURL, which=3, as.data.frame = TRUE,
                            skip.rows=1, header = TRUE, stringAsFctors = FALSE)
head(presidents)
tail(presidents)
tail(presidents$YEAR)

# take first 64 rows
presidents <- presidents[1:64, ]

library(stringr)

# Split up the year
yearList <- str_split(string = presidents$YEAR, pattern = "-")
head(yearList)

# Combine into matrix
yearMatrix <- data.frame(Reduce(rbind, yearList))
head(yearMatrix)
names(yearMatrix) <- c("Start", "Stop")
head(yearMatrix)

# Combine them
presidents <- cbind(presidents, yearMatrix)
head(presidents)
tail(presidents)


# Get first three characters of presidents names
str_sub(string = presidents$PRESIDENT, start = 1, end =3)

# Find all who's first year was a 1
presidents[str_sub(string = presidents$Start, start = 4, end = 4) == 1,
           c("YEAR", "Start", "Stop")]

# Any pres w/ John in name
str_detect(presidents$PRESIDENT, ignore.case("john"))

# Use it as a subset
presidents[str_detect(presidents$PRESIDENT, ignore.case("john")),]

# More complicated regular expressions
con <- url("http://www.jaredlander.com/data/warTimes.rdata")
load(con)
close(con)

# Find any times where separator is a dash
warTimes[str_detect(string = warTimes, pattern = "-")]
theTimes <- str_split(string = warTimes, pattern = "(ACAEA)|-", n = 2)
head(theTimes)

# Grab starting time
theStart  <- sapply(theTimes, FUN = function(x) x[1])
head(theStart)

# String trailing spaces
theStart <- str_trim(theStart)
head(theStart)

# Extract all occurances of jan
str_extract(string = theStart, pattern = "January")
theStart[str_detect(string = theStart, pattern = "January")]

# Extract only numbers where there is four in a row
# --- USe [0-9] to match any numeric occurances
head(str_extract(string = theStart, pattern = "[0-9][0-9][0-9][0-9]"), 20)

# USe shortcut ("\\d{4}" == first four occurances of digits)
head(str_extract(string = theStart, pattern = "[0-9]{4}"), 20)
head(str_extract(string = theStart, pattern = "\\d{4}"), 20)
head(str_extract(string = theStart, "\\d{1,3}"), 20)

# Find entries where four digit entry is at beginning of string
# - ^ means search beginning of line
head(str_extract(theStart, "^\\d{4}"), 30)

# Same as above but only thing on line
head(str_extract(theStart, "^\\d{4}$"), 30)

#Sub in text in the first match of a digit
head(str_replace(string = theStart, pattern = "\\d", replacement = "x"), 30)
head(str_replace_all(theStart, pattern = "\\d", "X"), 30)

# Replace 
head(str_replace_all(string = theStart, pattern = "\\d{1,4}", "x"), 30)


# Make new
commands <- c("<a href = index.hmlt>The link is here</a>", 
              "<b>This is bold text</b>")

# . is wildcard below; + is any amount of wildcards
# //1 goes to first grouping of paratheses and reinserts that
str_replace(string = commands, pattern="<.+?>(.+?)<.+>", replacement = "\\1")




