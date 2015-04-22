


# --- This is the latest we worked out with Kay (dec 2013)
# We were passing every non-glint pixel
#SSTF.BTRANGE <- !is.na(orig$cen.4050) &
#  !is.na(orig$cen.3959) &
#  !is.na(orig$cen.3750) &
#  !is.na(orig$cen.11000) &
#  !is.na(orig$cen.12000) & 
#  (((orig$glint < 0.005 | orig$solz >= 90 &
#  (orig$cen.11000 >= -4.0) & (orig$cen.11000 <= 37.0) &
#  (orig$cen.12000 >= -4.0) & (orig$cen.12000 <= 37.0) & 
#  (orig$cen.3750 >= -4.0) & (orig$cen.3750 <= 37.0) &
#  (orig$cen.3959 >= -4.0) & (orig$cen.3959 <= 37.0) &
#  (orig$cen.4050 >= -4.0) & (orig$cen.4050 <= 35.0))) |
#  (orig$solz < 90 & orig$glint > 0.005) &
#  (orig$cen.11000 >= -4.0) &
#  (orig$cen.12000 >= -4.0) &
#  (orig$cen.3750 >= -4.0) &
#  (orig$cen.3959 >= -4.0) & 
#  (orig$cen.4050 >= -4.0))

# Sue thinks this is correct

dd1 <- !is.na(orig$cen.4050) &
  !is.na(orig$cen.3959) &
  !is.na(orig$cen.3750) &
  !is.na(orig$cen.11000) &
  !is.na(orig$cen.12000)

dd2a <- (orig$glint < 0.005) &
  (orig$cen.11000 >= -4.0) & (orig$cen.11000 <= 37.0) &
  (orig$cen.12000 >= -4.0) & (orig$cen.12000 <= 37.0)

dd2b <- (orig$glint < 0.005) & 
  (orig$cen.3750 >= -4.0) & (orig$cen.3750 <= 37.0) &
  (orig$cen.3959 >= -4.0) & (orig$cen.3959 <= 37.0) &
  (orig$cen.4050 >= -4.0) & (orig$cen.4050 <= 35.0)

table(dd2a & dd2b)



dd3 <- ((orig$solz >= 90) &
          (orig$cen.11000 >= -4.0) & (orig$cen.11000 <= 37.0) &
          (orig$cen.12000 >= -4.0) & (orig$cen.12000 <= 37.0) & 
          (orig$cen.3750 >= -4.0) & (orig$cen.3750 <= 37.0) &
          (orig$cen.3959 >= -4.0) & (orig$cen.3959 <= 37.0) &
          (orig$cen.4050 >= -4.0) & (orig$cen.4050 <= 35.0))

dd4 <- ((orig$solz < 90 & orig$glint > 0.005) &
          (orig$cen.11000 >= -4.0) &
          (orig$cen.12000 >= -4.0) &
          (orig$cen.3750 >= -4.0) &
          (orig$cen.3959 >= -4.0) & 
          (orig$cen.4050 >= -4.0))

dd4a <- (orig$solz < 90 & orig$glint > 0.005) &
  ((orig$cen.3750 >= -4.0) &
     (orig$cen.3959 >= -4.0) & 
     (orig$cen.4050 >= -4.0))

dd4b <- (orig$solz < 90 & orig$glint > 0.005) &
  ((orig$cen.3750 < -4.0) |
     (orig$cen.3959 < -4.0) | 
     (orig$cen.4050 < -4.0))


#--------------------------
# Daytime and glint
table(ss5)
table(dd4)

# Daytime and glint tests
#ss5
#FALSE    TRUE 
#2237916  398296 
#dd4
#FALSE    TRUE 
#2344710  291440 
#398296 - 291440 = 106856
# 106856 passed ss5 and not dd4


# Night tests
# These matched
# 790838 passed
table(dd3) 
table(ss2 & ss3)


# ------------------
# Daytime and NOT glint
table(ss4)
table(dd2) # not glint
table(orig$solz < 90) # day

#ss4
#FALSE    TRUE 
#1999116  637053 
#dd2
#FALSE    TRUE 
#1320287 1315787
# 1315787 - 637053 =  678734
# 678734 passed dd2 but not ss4

# daytime & not glint & visir not too cold
# (should be same as ss4 but ss4 passees 7716 more
#637053 - 629337 = 7716

table(dd2b & orig$solz < 90)

table(dd1 & (dd2 | dd3 | dd4)) # btrange2



# Chunked 

ss1 <- !is.na(orig$cen.4050) &
  !is.na(orig$cen.3959) &
  !is.na(orig$cen.3750) &
  !is.na(orig$cen.11000) &
  !is.na(orig$cen.12000)

ss2 <- (orig$cen.11000 >= -4.0) & (orig$cen.11000 <= 37.0) &
  (orig$cen.12000 >= -4.0) & (orig$cen.12000 <= 37.0) 

ss3 <- ((orig$solz >= 90) &
  (orig$cen.3750 >= -4.0) & (orig$cen.3750 <= 37.0) &
  (orig$cen.3959 >= -4.0) & (orig$cen.3959 <= 37.0) &
  (orig$cen.4050 >= -4.0) & (orig$cen.4050 <= 35.0))

ss4 <- ((orig$solz < 90 & orig$glint < 0.005) &
  (orig$cen.3750 >= -4.0) &
  (orig$cen.3959 >= -4.0) & 
  (orig$cen.4050 >= -4.0))

ss5 <- (orig$solz < 90 & orig$glint > 0.005)

table(ss1 & ss2 & (ss3 | ss4 | ss5))

