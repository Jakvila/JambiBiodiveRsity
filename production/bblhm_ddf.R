


library(lpSolveAPI)


# Set direction vector
gx <- c(1,1,1)
gy <- 1

shifters <- c("timetrend","smallfsize","mediumfsize","largefsize")


estdat <- data.frame("y" = PalmDataRestricted$Production, "Xlabor" = PalmDataRestricted$Labour,
                     "Xmaterial" = PalmDataRestricted$Inputs, "Xsize" = PalmDataRestricted$Size)

# Estimate deterministic input-output quadratic DDF
ioddf <- ddfquad(yName = "y", xNames = c("Xlabor","Xmaterial","Xsize"), data = estdat, gx = gx, gy = gy)
# Estimate deterministic input quadratic DDF
iddf <- ddfquad(yName = "y", xNames = c("Xlabor","Xmaterial","Xsize"), data = estdat, gx = gx, gy = (0*gy))
# Estimate deterministic output quadratic DDF
oddf <- ddfquad(yName = "y", xNames = c("Xlabor","Xmaterial","Xsize"), data = estdat, gx = (0*gx), gy = gy)

# DDF with conditions of Lemma 2
ioddfAFT <- ddfquad(yName = "y", xNames = c("Xlabor","Xmaterial","Xsize"),  data = estdat, gx = gx, gy = gy, isAFT = TRUE)

# zeta = 2.072
sum(ioddfAFT$coeff[grep("ah", colnames(ioddfAFT$coeff))]*gx)

# Extract iddf from AFT condition: rescale coefficients and recompute inefficiency score!
iddfAFT <- ioddfAFT
iddfAFT$coeff <- iddfAFT$coeff/sum(iddfAFT$coeff[grep("ah", colnames(iddfAFT$coeff))]*gx)
iddfAFT$params$gy <- 0*gy
iddfAFT$Deff <- predict(iddfAFT, estdat)
iddfAFT$e <- sum(iddfAFT$Deff)
#colSums(gradients(iddfAFT, estdat) >= 1e-16) # Check monotonicity constraints
# Assign oddf from ioddf with AFT condition imposed
oddfAFT <- ioddfAFT
oddfAFT$params$gx <- 0*gx


# Check that OLS regression shows a perfect linear fit with coefficient = zeta = 2.072
lmaftres <- lm(oDeff ~ iDeff, data = data.frame(oDeff = oddfAFT$Deff, iDeff = iddfAFT$Deff))
summary(lmaftres)
lmaftres$coefficients["iDeff"] - sum(ioddfAFT$coeff[grep("ah", colnames(ioddfAFT$coeff))]*gx)

rm(ioddfAFT, lmaftres)

p1 <- ggplot(data = tidyr::gather(data.frame("iDDFAFT" = iddfAFT$Deff, "iDDF" = iddf$Deff))) +
  geom_histogram(aes(x = value, group = key, color = key, alpha = 0.2, fill = key), bins = 300) +
  xlab("Inefficiency") +
  ylab("Count") +
  labs(title = "Histogram of inefficiencies") +
  scale_alpha_continuous(guide = FALSE) +
  scale_color_discrete(guide = FALSE) +
  scale_fill_discrete(name = "", labels = c("D(x,y;g^i,0) + Lemma 2","D(x,y;g^i,0)")) + theme_bw()

p2 <- ggplot(data = tidyr::gather(data.frame("oDDFAFT" = oddfAFT$Deff, "oDDF" = oddf$Deff))) +
  geom_histogram(aes(x = value, group = key, color = key, alpha = 0.2, fill = key), bins = 300) +
  xlab("Inefficiency") +
  ylab("Count") +
  labs(title = "Histogram of inefficiencies") +
  scale_alpha_continuous(guide = FALSE) +
  scale_color_discrete(guide = FALSE) +
  scale_fill_discrete(name = "", labels = c("D(x,y;0,g^o) + Lemma 2","D(x,y;0,g^o)")) + theme_bw()
gridExtra::grid.arrange(p1,p2)
