# ============================
# RSS Reader & Search Hit Tool
# ============================

library("tibble")
library("stringr")
library("tidyRSS")
library("sendmailR")

msg = "Relevant articles: \n\n"

# Feed links with title and link column numbers 
rssLink <-tribble(
  ~link, ~titleCol, ~linkCol,
  "theregister.co.uk/feeds/latest.rdf",6,8,
  "www.heise.de/newsticker/heise-atom.xml",5,7,
  "https://krebsonsecurity.com/feed/",7,15
)

# Cycle thru 3 RSS feeds
for (n in 1:3){
  
  feed <- tidyfeed(as.matrix(rssLink[n,1]))
  
  feed2 <- feed[,c(as.matrix(rssLink[n,2]),as.matrix(rssLink[n,3]))]
  
  feed2$lowCsTitle <- mapply(str_to_lower, as.matrix(feed2[,1]))
  
  hitRow <- grepl("android | google", as.matrix(feed2[,3]))
  
  # Vector of hits
  if (sum(hitRow) == 0){
  } else if (sum(hitRow) > 0) {
    hits <- paste(feed2[hitRow,1], "\n", feed2[hitRow,2], "\n\n")
  }
  
  # Add vector rows to msg
  if (sum(hitRow) == 0){
  } else if (sum(hitRow) > 0) {
    for (i in 1:sum(hitRow)) {
      msg <- paste(msg, hits[i])
    }
  }
  
} # end outer for loop


# =======================
# Send Notification Email
# =======================
from <- "<fromEml>"
to <- "<toEml>"
subject <- "Relevant Articles"

if (msg != "") {
  sendmail(from, to, subject, msg)
}












