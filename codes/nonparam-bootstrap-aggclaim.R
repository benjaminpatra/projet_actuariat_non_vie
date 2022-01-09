
#### simulation de charge sinistre portefeuille non parametrique ####
rclaimnb <- function(n, mydata, var="claim_nb")
{
  nbrow <- NROW(mydata)
  rclaimnb1 <- function()
  {
    rrow <- sample.int(nbrow, replace=TRUE)
    sum(mydata[rrow, var])
  }
  replicate(n, rclaimnb1())
}
#rclaimnb(6, freMTPLfreq)

rclaimagg <- function(n, mydatanb, mydataamount, varnb="claim_nb", varamount="claim_amount")
{
  nbrow <- NROW(mydataamount)
  ragg1 <- function(nbclaim)
  {
    rrow <- sample(nbrow, size=nbclaim, replace=TRUE)
    sum(mydataamount[rrow, varamount])
  }
  rnb <- rclaimnb(n, mydatanb, var=varnb)
  sapply(rnb, ragg1)
}
#essai
#rclaimagg(6, freMTPLfreq, freMTPLsev)
#10 000 simu
#rbootclaimagg <- rclaimagg(1e4, freMTPLfreq, freMTPLsev)

