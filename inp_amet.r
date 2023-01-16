# inp_amet.r
# import import winds and other variables from AERMET surface meteorological data input file
# sjb 2022-05-16

# ############################################################

# inp_amet()
# import AERMOD/AERMET surface file meteorological variables
# calls line1_amet

# line1_amet() - import header 1st line of AERMET surface file
# import AERMOD/AERMET surface file header line
# calls longlatnum

# longlatnum() 
# change latitude or longitude ending in E/W/S/N to a number
# Formerly line1_amet and longlatnum were in separate file line1_amet.r

# ############################################################
# Function:  inp_amet(filename, tz, drop.ymdh=TRUE, wind.only=TRUE, put.NA=TRUE)
# Purpose:   import AERMOD/AERMET surface file meteorological variables
# Arguments: file name, name of meteorological data file
#            tz, UTC adjustment (-8), or time zone string ("Etc/GMT+8"),
#              or "file" to deduce from header 1st line of file
#            drop.ymdh, Boolean, whether to drop individual year, month, etc.
#            wind.only, Boolean, whether to keep only wind speed and direction
#            put.NA, Boolean, whether to replace missing values with NA
#            by.rows, Boolean, whether replace NA by rows instead of columns
#               Result is same either way, but by.rows=FALSE is 30x faster.
#            raw, Boolean, whether to just import, no data changes
# Returns:   data from with wind direction, wind speed, datetime
# Side effects: None
# Exceptions:   warning for unknown timezone
# Comments: tz is required; originally had default tz='Etc/GMT+8'
#           calls line1_amet
# External:    None
# Optional:  
#
# #######################################################
# To do
# specify variables to import
# allow absent time zone?
# profile data (.PFL)?

# Uses read.table
# for 5-year met. file, raw=TRUE read took <1 sec; read.fwf took 18 seconds.

# ============================================================

# #######################################################
# AERMET surface format; more below at end 
# 9  1  1   1  1 -999.0 -9.000 -9.000 -9.000 -999. -999. -99999.0  0.1182   0.88   1.00    0.00    0.0   10.0  279.1    2.0     0   0.00    86.  1019.    10 ADJ-SFC NoSubs      
#yy mm dd jjj hr      H     u*     w*   VPTG   Zic   Zim        L      z0     B0      r      Ws     Wd   zref   temp  ztemp ipcod   pamt     rh   pres  ccvr   WSADJ         Subs
#

#setwd('M:/Documents/R/projects/aerm_io')
# source("inp_amet.r")
#ametfile <- '724828_Edited.SFC'
#ametfile <- '724828_short.SFC'
# amet <- inp_amet(ametfile, tz="Etc/GMT+8")
# amet <- inp_amet(ametfile, tz="Etc/GMT+8", drop.ymdh=TRUE, wind.only=TRUE, put.NA=FALSE)
# amet <- inp_amet(ametfile, tz="Etc/GMT+8", drop.ymdh=TRUE, wind.only=FALSE, put.NA=TRUE)
# write.csv(amet, "short1.csv")
# write.csv(amet, "long.csv")

# Notes for read.table()
#skip 1st line
#single digits for yy, nn, dd, hh if under 10
#Missing times are not allowed
#sep="" default: one or more spaces or tabs
#columns 1-5 are time-related; (then 10 columns unused); 15 and 16 are Ws and Wd (then 10 more columns)


inp_amet <- function(ametfile, tz, drop.ymdh=TRUE, wind.only=TRUE, 
    put.NA=TRUE, by.rows=FALSE, raw=FALSE) {

if (missing(tz)) {
    stop(paste(sep="", "tz (time zone) is required. ",
         "E.g. -8 or 'Etc/GMT+8' for UTC-08:00, Los Angeles; ",
         "may be 'file' to deduce from station longitude in file line 1, ",
         "or '' to leave unspecified."))
} else { 
    # Sign convention for tz database "Etc/GMT" is opposite of ISO 8601 
    if (is.numeric(tz)) {
        tz <- paste("Etc/GMT",  ifelse(tz<=0, "+", ""), -tz, sep="")
    } else {
      if (tolower(tz) == "file")
        tz <- line1_amet(ametfile)[["tz"]]
    }
}

if (raw) {
    wind.only <- FALSE
    put.NA <- FALSE
    drop.ymdh <- FALSE
}

metvar.names <- c(
    "yr", "mo", "dy", "julian", "hr", 
    "hsensible", "ustar", "wstar", "vptg", "zic",
    "zim", "molen", "z0", "bowen", "albedo", 
    "ws", "wd", "zref", "temp", "ztemp",
    "ipcod", "pamt", "rh", "pres", "ccvr", 
    "wsadj", "subs"
)

# each variable has particular value that means missing
# From AERMET user guide Appendix B, and an example surface output file
metvar.missing.vals <- c(
    NA, NA, NA, NA, NA, 
    -999.0, -9.000, -9.000, -9.000, -999, 
    -999, -99999.0, -9.0000, -9.00, -9.00, 
    999.00, 999.0, -9.0, 999.0, -9.0, 
    9999, -9.00, 999, 99999, 99, 
    NA, NA
)  
# FALSE becomes 0, which could be a valid value...
# Can't use NA in index vector to select amet columns;
#   must fix or skip later

names(metvar.missing.vals) <- metvar.names

if (wind.only) {
    amet <- read.table(ametfile, 
        header=FALSE, sep="", skip=1, flush=TRUE,
        col.names   = c(
            "yr", "mo", "dy", "julian", "hr", 
            rep("na",   10), 
            "ws", "wd",  rep("na",   10)),
        colClasses  = c(
            rep("integer", 5),
            rep("NULL", 10), 
            "numeric", "numeric", rep("NULL", 10))
    )
    # error if not all fields are described
        #  na.strings=c("999.00", "999.0"),

    if (put.NA) {
        # by variables/columns  (cf. not by.rows)
        # In rows for which variable matches 'missing',
        #   set that variable to NA
        amet[ amet[ , "ws"]==999, "ws"] = NA
        amet[ amet[ , "wd"]==999, "wd"] = NA
    }

} else {
    amet <- read.table(ametfile, 
        header=FALSE, sep="", skip=1, flush=TRUE, 
        col.names   = metvar.names,
        colClasses  = c(rep("integer", 5), 
            rep("numeric", 15), 
            "integer", rep("numeric", 3),  "integer", 
            "character", "character")
    )
    
    if (put.NA) {
        if (by.rows) {
            for (i in seq(1, nrow(amet)) ) {
                # In variables equal to corresponding 'missing' value, put NA
                amet[i, 
                    amet[i, ] == metvar.missing.vals
                        & !is.na(metvar.missing.vals)
                ] <- NA
                #missing.lidx <<- amet[i, ] == metvar.missing.vals
                #amet[i, missing.lidx] <- NA
            }
        } else {
            # loop over variables/columns
            for (j in seq(1, ncol(amet))) {
                if (is.na(metvar.missing.vals[j])) next

                # In elements equal to 'missing' value for that variable j, put NA
                amet[ (amet[ , j] == metvar.missing.vals[j]) ,j] <- NA

                #missing.lidx <<- amet[ , j] == metvar.missing.vals[j]
                #amet[missing.lidx , j] <- NA
            }
        }  # end if by.rows
    }  # end if put.NA
}  # end if wind.only

# If don't specify the unused ending column classes:
# Error in read.table(ametfile, header = FALSE, sep = "", skip = 1, flush = TRUE,  : 
#  more columns than column names

# Clean up data
# adjust from 2-digit to 4-digit year
# change hours from 1-24 to 0-23
# assign time zone (needed?)
# ISOdatetime(year, month, day, hour, min, sec, tz = "")

if (! raw) {
    amet <- cbind( 
    # add 'date' before existing variables
        date = ISOdatetime(
            ifelse(amet$yr >= 50, amet$yr + 1900, amet$yr + 2000), 
            amet$mo, amet$dy, amet$hr - 1, 0, 0, tz=tz),
        amet)
            
#    # add 'date' after exising variables
#    amet[["date"]] <- ISOdatetime(
#            ifelse(amet$yr >= 50, amet$yr + 1900, amet$yr + 2000), 
#            amet$mo, amet$dy, amet$hr - 1, 0, 0, tz=tz)
}

# Per warning in 'within' documentation, avoiding it.
#amet <- within(amet, date <- ISOdatetime(ifelse(yr >= 50, yr+1900, yr+2000), mo, dy, hr-1, 0, 0, tz=tz))

if (drop.ymdh) {
    # drop time variables no longer needed after date was created
    amet[c("yr", "mo", "dy", "julian", "hr")] <- NULL
}

return(amet)

} # end function inp_amet

# ############################################################
# Function:  line1_amet(filename)
# Purpose:   import AERMOD/AERMET surface file header line
# Arguments: file name, name of meteorological data file
# Returns:   list of header info, guess at UTC offset, and timezone string
# header info: latitude & longitude strings, 
#   UA identifier, SF identifier, OS identifier, AERMET Version date
# Side effects: None
# Exceptions:   None
# Comments: calls longlatnum
# External:    None
# Optional:  
#
# #######################################################
# To do
# ============================================================

# AERMET user guide sec C.4
# FORMAT (2(2X,A8), 8X,' UA_ID: ',A8, ' SF_ID: ',A8, ' OS_ID: ',A8, T85, 'VERSION:', A6 )
#    38.367N  121.950W          UA_ID:    23230  SF_ID:    93241  OS_ID:              VERSION: 14134    CCVR_Sub TEMP_Sub
# These are all written as character fields, even though ID's are often integers

line1_amet <- function(ametfile) {

aline1 <- read.fwf(ametfile, n=1,
      widths = c(-2, 8, -2, 8, -8, 9, 8, 9, 8, 9, 8, -5, 8, 6, 200),
      col.names = c("lat.str", "long.str", 
          "UAstr", "UA.ID", "SFstr", "SF.ID", "OSstr", "OS.ID", 
          "AMET.VERstr", "AMET.VER", "Processing"),
      colClasses = rep("character", 11)
)
#ID.strings <- aline1[1, c(3, 5, 7)]
#ID.strings <-sub(' *([A-Za-z_]+): *', "\\1", ID.strings)
##ID.strings <-sub('_', '.', ID.strings, fixed=TRUE)

ahead <- as.list(aline1[1, c(1, 2, 4, 6, 8, 10, 11)])

for (i in seq(1, length(ahead))) ahead[[i]] <- trimws(ahead[[i]])
# as.list(trimws()) removes names; ditto trimws(as.list())

# convert lat/long strings to numbers, accounting for trailing E/W/S/N
longitude <- longlatnum(ahead[["long.str"]])
latitude <- longlatnum(ahead[["lat.str"]])

# guess time zone from longitude
utc <- ceiling((longitude-7.5)/15)
tz <- paste("Etc/GMT", ifelse(utc<=0, "+", ""), -utc, sep="")
# 'Etc/GMT+8'

return(c(ahead, list(longitude=longitude, latitude=latitude, 
            UTC=utc, tz=tz)))

#station.info <- list()
#station.info <- list(head=ahead, longitude=longitude, UTC=utc, tz=tz)
#return(station.info)

} # end function line1_amet


# ############################################################
# Function:  longlatnum(llstring)
# Purpose:   change latitude or longitude ending in E/W/S/N to a number
# Arguments: string, latitude or longitude ending in E/W/S/N
# Returns:   latitude or longitude number
# Side effects: None
# Exceptions:   none, but returns NA if last character not in [EWSN\d.]
# Comments:  If input has both '-' and W or S, they cancel: -120W -> 120
# External:    None
# Optional:  
#
# #######################################################

longlatnum <- function(llstring) {

  nc.ll <- nchar(llstring)
  end.char <- toupper(substr(llstring, nc.ll, nc.ll))
  
  if (regexpr("[EWSN[:digit:].]", end.char) == -1) {
    warning(paste("latitude or longitude string '", llstring, 
                  "' did not end in E,W,S,N, or digit", sep=""))
    return(NA)
  }
  
  if (end.char == "W" || end.char == "S") {
    llnum <- -as.numeric(substr(llstring, 1, nc.ll-1))
  } else {
    if (end.char == "E" || end.char == "N") {
      llnum <- as.numeric(substr(llstring, 1, nc.ll-1))
    } else {
      llnum <- as.numeric(llstring)
    }
  }
  return (llnum)
} # end function longlatnum

# #######################################################

# ============================================================
# AERMET surface meteorology file for AERMOD
#AERMET 21112
#MPOUT.FOR
#line 124
#            WRITE( DEV80,1800 ) MPYR,MPCMO,MPCDY,MPJDY,J,
#     &           HFLUX(J),
#     &           USTAR(J),WSTAR(J),VPTG(J),ZICONV(J),ZIMECH(J),MOL(J),
#     &           Z0(J),BOWEN(J),ALBEDO(J),WSPD(J),WDIR(J),ZREF(J),
#     &           T(J),ZTREF(J), IPCODE(J), PAMT(J), RH(J),P(J),CCVR(J),  ! dtb #300 03071
#     &           WSADJ, SUBs
#
# 1800       FORMAT( 3(I2,1X), I3,1X, I2,1X, F6.1,1X, F6.3,1X, F6.3,1X,
#     &           F6.3,1X, 2(F5.0,1X),
#     &           F8.1,1X, F7.4,1X, F6.2,1X, F6.2,1X, F7.2,1X, F6.1,
#     &           3(1X,F6.1), 1X,I5, 1X,F6.2, 2(1X, F6.0), 1X, I5,        ! dtb #011 01180
#     &           1X, A7, 1X, A12)
#
# The F7.2,1X, F6.1 is for wd and ws; user guide has F5.0 for ws.


