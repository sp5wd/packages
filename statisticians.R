setwd("~/STAT3280")
install.packages("RSQLite", dependencies = T)

install.packages("tm")
library(tm)

setwd("~/STAT3280/statisticians")
setwd("~/STAT3280/statisticians/Data")


dt <- scan("paperList_Abstracts_Keyword.txt",what="",sep="\n")
length(dt)
dt <- dt[-1]

tmp1 <- lapply(dt,function(s)unlist(strsplit(s,'"')))
years <- unlist(lapply(tmp1,function(x) return(x[3])))
years <- gsub(",","",years)
years <- as.numeric(years)

abss <- unlist(lapply(tmp1,function(x) return(x[4])))
abss <- unlist(lapply(abss,function(x)gsub('\\{','',x)))
abss <- unlist(lapply(abss,function(x)gsub("}","",x)))
abss <- unlist(lapply(abss,function(x)gsub("#","",x)))
abss <- unlist(lapply(abss,function(x)gsub("&","",x)))
abss <- unlist(lapply(abss,function(x)gsub("<","",x)))
abss <- unlist(lapply(abss,function(x)gsub(">","",x)))
abss <- unlist(lapply(abss,function(x)gsub("/","",x)))
abss <- unlist(lapply(abss,function(x)gsub(")","",x)))
abss <- unlist(lapply(abss,function(x)gsub("\\(","",x)))
abss <- unlist(lapply(abss,function(x)gsub("amp","",x)))

# DON'T RERUN
# make a folder called “paper abstracts” in the data folder before running this
#for(k in 1:length(abss)){
 # write(abss[k], file = paste("./PaperAbstracts/",k,sep=""))
#}

# reread in the abstracts as a corpus
cname <- "./PaperAbstracts"
docs <- Corpus(DirSource(cname))   
document.names <- unlist(meta(docs,"id"))

new.docs <- tm_map(docs, removePunctuation)
docs <- new.docs
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)

# might need to run this line multiple times for it to work
docs <- tm_map(docs, removeWords, stopwords("english"))

new.docs <- tm_map(docs, stripWhitespace)
docs <- new.docs

# correct years
document.names <- as.numeric(document.names)
correct.years <- rep(NA,length(years))
for(i in 1:length(years)){
  correct.years[i] <- years[document.names[i]]
}

dtm <- DocumentTermMatrix(docs)
dtms <- removeSparseTerms(dtm, 0.99)
DTM <- as.matrix(dtms)

doc.index <- as.numeric(document.names)

final.DTM <- matrix(0,nrow=nrow(DTM),ncol=ncol(DTM+1))
final.DTM[doc.index,] <- DTM

colnames(final.DTM) <- colnames(DTM)
rownames(final.DTM) <- rownames(DTM)
final.DTM <- cbind(correct.years,final.DTM)

# part 2
A2P <- read.table("authorPaperBiadj.txt")

rownames(A2P)
A2P <- as.matrix(A2P)
A.DTM <- A2P%*%final.DTM[,-1]
dim(A.DTM)

authors <- read.table("authorList.txt",stringsAsFactors=FALSE)
head(authors)
authors <- unlist(lapply(authors,function(x)gsub(" ","_",x)))
head(authors)

length(authors)

rownames(A.DTM)  <- authors

Coauthor.Adj <- A2P%*%t(A2P)

dim(Coauthor.Adj)
A.DTM[1:6,1:6]

P2C <-read.table("paperCitAdj.txt")
P2C <- as.matrix(P2C)
A2C <- A2P%*%P2C

dim(A2C)
rownames(A2C) <- authors
colnames(A2C) <- NULL
rownames(A2P) <- authors

# clean up paper list for years
paper_list <- scan("paperList.txt",sep="\n",what="")
paper_list <- paper_list[-1]
tmp1 <- lapply(paper_list,function(s)unlist(strsplit(s,'"')))
head(tmp1)
years <- unlist(lapply(tmp1,function(x) return(x[5])))
years <- gsub(",","",years)
years <- as.numeric(years)



#Question Two:
  install.packages("RSQLite")
library(RSQLite)
install.packages("stringi")
library(stringi)

# make the database
db = dbConnect(SQLite(), dbname = "AuthorsPapers.sqlite")

# authors table!
dbSendQuery(conn = db, "CREATE TABLE AUTHORS (author_id INTEGER, first_name TEXT, last_name TEXT)")
authorsTable <- data.frame(matrix(ncol = 3, nrow = length(authors)))
colnames(authorsTable) <- c("author_id", "first_name", "last_name")
for(i in 1:length(authors)){
  stri_split_fixed(stri_reverse(authors[i]),"_",n = 2)
  stri_reverse(stri_split_fixed(stri_reverse(authors[i]),"_",n = 2)[[1]])
  AuthorName <- stri_reverse(stri_split_fixed(stri_reverse(authors[i]),"_",n = 2)[[1]])[2:1]
  authorsTable[i, 1] <- i
  authorsTable[i, 2] <- AuthorName[1]
  authorsTable[i, 3] <- AuthorName[2]
}
dbWriteTable(conn = db, name = "AUTHORS", authorsTable, overwrite = TRUE, row.names = FALSE)
head(dbReadTable(db, "AUTHORS"))

# papers table!
dbSendQuery(conn = db, "CREATE TABLE PAPERS (paper_id INTEGER, DOI REAL, year INTEGER, title TEXT, citation_count INTEGER, abstract TEXT)")
papersTable <- data.frame(matrix(ncol = 6, nrow = length(tmp1)))
colnames(papersTable) <- c("paper_id", "DOI", "year", "title", "citation_count", "abstract")
for(i in 1:length(tmp1)){
  papersTable[i, 1] <- i
  papersTable[i, 2] <- strsplit(tmp1[[i]][4], "/")[1]
  papersTable[i, 3] <- as.numeric(strsplit(tmp1[[i]][5], ",")[[1]][2])
  papersTable[i, 4] <- tmp1[[i]][6]
  papersTable[i, 5] <- as.numeric(strsplit(tmp1[[i]][7], ",")[[1]][2])
  papersTable[i, 6] <- abss[i+1]
}
dbWriteTable(conn = db, name = "PAPERS", papersTable, overwrite = TRUE, row.names = FALSE)
head(dbReadTable(db, "PAPERS"))

# author_paper table!
dbSendQuery(conn = db, "CREATE TABLE AUTHOR_PAPER (row_id INTEGER, author_id INTEGER, paper_id INTEGER)")
authorPaperTable <- data.frame(matrix(ncol = 3))
colnames(authorPaperTable) <- c("row_id", "author_id", "paper_id")
k <- 1
for(i in 1:3607){
  for(j in 1:3248){
    if(A2P[i,j] == 1){
      authorPaperTable[k, 1] <- k
      authorPaperTable[k, 2] <- i
      authorPaperTable[k, 3] <- j
      k <- k+1
    }
  }
}
dbWriteTable(conn = db, name = "AUTHOR_PAPER", authorPaperTable, overwrite = TRUE, row.names = FALSE)
head(dbReadTable(db, "AUTHOR_PAPER"))

# citations table!
dbSendQuery(conn = db, "CREATE TABLE CITATIONS (row_id INTEGER, paper1_id INTEGER, paper2_id INTEGER)")
citationsTable <- data.frame(matrix(ncol = 3))
colnames(citationsTable) <- c("row_id", "paper1_id", "paper2_id")
k <- 1
for(i in 1:3248){
  for(j in 1:3248){
    if(P2C[i,j] == 1){
      citationsTable[k, 1] <- k
      citationsTable[k, 2] <- i
      citationsTable[k, 3] <- j
      k <- k+1
    }
  }
}
dbWriteTable(conn = db, name = "CITATIONS", citationsTable, overwrite = TRUE, row.names = FALSE)
head(dbReadTable(db, "CITATIONS"))

###############################################################################################
##Questions 3
# authors table
dbSendQuery(conn = db, "CREATE TABLE AUTHORS (author_id INTEGER, first_name TEXT, last_name TEXT)")
head(dbReadTable(db, "AUTHORS"))


# author_paper table
dbSendQuery(conn = db, "CREATE TABLE AUTHOR_PAPER (row_id INTEGER, author_id INTEGER, paper_id INTEGER)")
head(dbReadTable(db, "AUTHOR_PAPER"))

# citations table
dbSendQuery(conn = db, "CREATE TABLE CITATIONS (row_id INTEGER, paper1_id INTEGER, paper2_id INTEGER)")
head(dbReadTable(db, "CITATIONS"))

# papers table
dbSendQuery(conn = db, "CREATE TABLE PAPERS (paper_id INTEGER, DOI REAL, year INTEGER, title TEXT, citation_count INTEGER, abstract TEXT)")
head(dbReadTable(db, "PAPERS"))
###############################################################################################

## Question 4
# need to extract all papers published before 2010 and count the number of times they are cited

dbGetQuery(db, 'SELECT paper_id, log(citation_count) FROM papers WHERE year in ("2003","2004","2005","2006","2007","2008","2009") AND citation_count !=  0')


res <- dbSendQuery(db,'SELECT DISTINCT (citation_count) FROM papers WHERE year in ("2003","2004","2005","2006","2007","2008","2009") AND citation_count !=  0')

results <- dbFetch(res)
results_total_log_citations <- log(results$citation_count)

hist(results_total_log_citations)


# dbGetQuery(db, 'SELECT DISTINCT(citation_count) FROM papers WHERE year in ("2003","2004","2005","2006","2007","2008","2009") AND citation_count !=  0')

res1 <- dbSendQuery(db,'SELECT DISTINCT(citation_count) FROM papers WHERE year in ("2003","2004","2005","2006","2007","2008","2009") AND citation_count !=  0')

results1 <- dbFetch(res1)

log_count_citation <- log(results1$citation_count)


plot(results_total_log_citations,log_count_citation) 



