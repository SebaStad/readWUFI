#' Guesses the Date format for the WUFIPlus DateColumn. Its pretty good at it.
#'
#' @param DateColumn WUFIPlus Datecolumn
#'
#' @return Character. v.Dateoformat is the string section for correct formatting of dates.
#' @export v.Dateformat Charac.ter
#'
#' @examples
#' x <- 1
f.GuessDate_for_GetWUFIplus <- function(DateColumn){
  # Der erste Abschnitt verwandelt die Datumszeile in einzelne Zeichen und erkennt
  # alle (bisher) denkbaren Sonderzeichen und gibt ihnen eine Position im
  # Datums Vektor
  stopifnot(is.vector(DateColumn))
  if(!is.vector(DateColumn)) stop("Function input must be a vector")
  simpleSep <- c("/",".", "\\", ",", " ", ":", "-")
  v.SingleChar <- strsplit(DateColumn[2], "") # Nummer Zwei wegen 01,07 format.
  Sonderzeichen <- c()
  Son.Position  <- c()
  for(j in simpleSep){
    SZ<- unlist(lapply(v.SingleChar, function(x) which(x == j)))
    Son.Position <- c(Son.Position , SZ)
    Sonderzeichen  <- c(Sonderzeichen,rep(j,length(SZ)))
  }
  Sonder.Pos <- data.frame(Sonder = Sonderzeichen,
                           Position = Son.Position)

  Sec.Mult    <- 1
  Min.Mult    <- 1
  # Funktioniert einwandfrei

  # Dieser Block versucht die interne Reihenfolge von Tag, Stunde, Monat und Jahr
  # herauszukriegen.
  # k ist 25 Reihen lang, die Position der Stunde erkennt man an dem Unterschiedlichen Beitrag
  # von FH (FirstHour) auf SH (SecondHour). Gibt wohl Probleme, wenn jemand seine Simulation
  # Um 23 Uhr eines Tages beginnt -> Weitere If-Schleife benötigt
  Get.Num.Pos    <- gregexpr("[0-9]+", DateColumn[1])
  Get.Num.Entries <- as.numeric(unlist(regmatches(DateColumn[1], Get.Num.Pos)))

  FT.NR           <- Get.Num.Pos                            # First Timestep
  FT.positions    <- Get.Num.Entries
  if(length(Get.Num.Entries)==6){
    SS.NR <- gregexpr("[0-9]+", DateColumn[1+1])                     # second Second
    SS.positions <- as.numeric(unlist(regmatches(DateColumn[2], SS.NR)))
    Guess.Second <- which(FT.positions!=SS.positions)
    Second.Step  <- abs(FT.positions[Guess.Second]-SS.positions[Guess.Second])

    TM.NR <- gregexpr("[0-9]+", DateColumn[60/Second.Step+2])                    # Test Minute
    TM.positions <- as.numeric(unlist(regmatches(DateColumn[60/Second.Step+2], TM.NR)))
    if(SS.positions[Guess.Second]==TM.positions[Guess.Second]){
      Second <- Guess.Second
      Sec.Mult <- 60/Second.Step
    }

    SM.NR <- gregexpr("[0-9]+", DateColumn[Sec.Mult+1])                # Second Minute
    SM.positions <- as.numeric(unlist(regmatches(DateColumn[Sec.Mult+1], SM.NR)))
    Guess.Minute   <- which(FT.positions!=SM.positions)
    Minute.Step    <- abs(FT.positions[Guess.Minute]-SM.positions[Guess.Minute])

    TH.NR <- gregexpr("[0-9]+", DateColumn[(Sec.Mult*60)/Minute.Step+1+Sec.Mult])           # Test Hour
    TH.positions <- as.numeric(unlist(regmatches(DateColumn[(Sec.Mult*60)/Minute.Step+1+Sec.Mult],
                                                 TH.NR)))
    tryCatch(
    if(SM.positions[Guess.Minute]==TH.positions[Guess.Minute]){
      Minute <- Guess.Minute
      Min.Mult <- 60/Minute.Step
    },error=function(e){})
    if(exists("Second") && !exists("Minute")){
      Minute <- Second
      rm(Second)
    }
  }
  if(length(Get.Num.Entries)==5){
    SM.NR <- gregexpr("[0-9]+", DateColumn[1+Sec.Mult])                # Second Minute
    SM.positions <- as.numeric(unlist(regmatches(DateColumn[1+Sec.Mult], SM.NR)))
    Guess.Minute   <- which(FT.positions!=SM.positions)
    if(length(Guess.Minute)>1){
      Guess.Minute.Step    <- abs(FT.positions[Guess.Minute]-SM.positions[Guess.Minute])
      if(any(Guess.Minute.Step>1) && any(Guess.Minute.Step==1) ){
        Which.Minute <- which(abs(FT.positions[Guess.Minute]-SM.positions[Guess.Minute])>1)
        Guess.Minute <- Guess.Minute[Which.Minute]
      }
    }
    Minute.Step    <- abs((SM.positions[Guess.Minute]-FT.positions[Guess.Minute])%%60)

    TH.NR <- gregexpr("[0-9]+", DateColumn[(Sec.Mult*60)/Minute.Step+1+Sec.Mult])           # Test Hour
    TH.positions <- as.numeric(unlist(regmatches(DateColumn[(Sec.Mult*60)/Minute.Step+1+Sec.Mult], TH.NR)))
    if(SM.positions[Guess.Minute]==TH.positions[Guess.Minute]){
      Minute <- Guess.Minute
      Min.Mult <- 60/Minute.Step
    }
  }

  if(length(DateColumn) <= Sec.Mult*Min.Mult*24) stop("Datevector is too short for format evaluation.
                                     Manual Input?")
    SH.NR <- gregexpr("[0-9]+", DateColumn[Min.Mult*Sec.Mult+1])
    SH.positions <- as.numeric(unlist(regmatches(DateColumn[Min.Mult*Sec.Mult+1], FT.NR)))
    Hour <- which(FT.positions!=SH.positions)
    if(length(Hour) > 1){ # Sollte normal 23 sein. ==23 ausreichend?
      Hour <- Hour[which(abs(SH.positions[Hour]- FH.positions[Hour])>=22 &
                           abs(SH.positions[Hour]- FH.positions[Hour])<=24 )]
    }


    # Der Tag wird erkannt, an der Änderung der Einträge 25 Stunden später. Das sollte für
    # so gut wie alle Fälle ausreichen sein (außer Simulation < 25h...)
    ND.NR <- gregexpr("[0-9]+", DateColumn[24*Min.Mult*Sec.Mult+1])
    ND.positions <- as.numeric(unlist(regmatches(DateColumn[24*Min.Mult*Sec.Mult+1], ND.NR)))
    Day <- which(FT.positions!=ND.positions)
    if(length(Day) >1){
      Day  <- Day[which(abs(FH.positions[Day]-ND.positions[Day])!= 1 &
                          abs(FH.positions[Day]-ND.positions[Day])!= 11 )]
    }

    # Die derzeit wackligste Stelle: ich such mir einen der restlichen Einträge der größer
    # 12 Ist. Sieht man von Minuten/Sekunden hier ab (Selten in WUFI Simulation) und dem Fall,
    # das die Simulation älter ist, funktioniert auch das sehr gut.
    # Bei den Monaten Mach ich die Annahme, dass sowohl etwaige Minuten und Sekunden = 0 sind.
    if(exists("Minute") && exists("Second")){
      SearchYear <-FT.positions[-c(Hour,Day,Minute, Second)]
    }
    if(exists("Minute") && !exists("Second")){
      SearchYear <-FT.positions[-c(Hour,Day,Minute)]
    }
    if(!exists("Minute") && !exists("Second")){
      SearchYear <-FT.positions[-c(Hour,Day)]
    }

    if(any(SearchYear>12)){
      Year <- which(FT.positions==SearchYear[which(SearchYear>12)])
      Month <- which(SH.positions==FT.positions)[-c(Hour,Day,Year)]
      if(length(Month)>1){
        if(any(SH.positions[Month]==0)){
          Month <- Month[which(SH.positions[Month]!=0)]
        } else if(exists("Second")){
          Month <- SH.positions[Month[which(Month!=Second)]]
        }
      }
    }else if(any(SearchYear==1) && any(SearchYear[-which(SearchYear==1)]<=12)){
      Month <- which(FT.positions==SearchYear[which(SearchYear==1)])
      if(length(Month)>1){
        Month <- Month[-c(Day,Hour)]
      }
      Year  <- which(!SH.positions==0)[-c(Hour,Day,Month)]
    }else if(Day==1){
      Month <- 2
      Year  <- 3
    }else if(Day==2){
      Month <- 1
      Year  <- 3
      }

    Lengths <- attributes(FT.NR[[1]])$match.length
    if(exists("Second") && exists("Minute")){
      Date.DF <- data.frame(second=rbind(Second,Lengths[Second]),
                            minute=rbind(Minute,Lengths[Minute]),
                            hour=rbind(Hour,Lengths[Hour]),
                            day=rbind(Day,Lengths[Day]),
                            month=rbind(Month,Lengths[Month]),
                            year=rbind(Year,Lengths[Year]),
                            row.names = c("order", "length"))
    }else if(exists("Minute") && !exists("Second")){
      Date.DF <- data.frame(minute=rbind(Minute,Lengths[Minute]),
                            hour=rbind(Hour,Lengths[Hour]),
                            day=rbind(Day,Lengths[Day]),
                            month=rbind(Month,Lengths[Month]),
                            year=rbind(Year,Lengths[Year]),
                            row.names = c("order", "length"))
    }else if(!exists("Minute") && !exists("Second")){
    Date.DF <- data.frame(hour=rbind(Hour,Lengths[Hour]),
                          day=rbind(Day,Lengths[Day]),
                          month=rbind(Month,Lengths[Month]),
                          year=rbind(Year,Lengths[Year]),
                          row.names = c("order", "length"))
  }


  # Abgesehen von den gemachten Annahmen, funktioniert der obrige Block zu erstellung des
  # Date.DF gut.

  # Sieht doch 1A aus oder? ;)
  # Stellen wir uns den v.SingleChar so vor:
  #  [1] "0" "1" "." "0" "1" "." "2" "0" "1" "5" " " "0" "0" ":" "0" "0" ":" "0" "0"
  #
  # d.h. Sonder.Pos$Position
  # [1]  3  6 11 14 17
  #
  # Die Schleife fügt also immer, wenn m in Sonder.Pos$Position das jeweilige
  # Sonderzeichen dazu. -> Easy
  #
  # date.counter zählt mit, wie oft eine Variable aus dem Date.DF aufgerufen wurde
  # Sie Beginnt bei 1 und wird in der ersten (zweiten) Schleife immer erhöht,
  # wenn m+1 nicht mehr in den Sonderzeichen ist (also eine Zahl kommt)
  #
  # In die untere Schleife kommt man nur, falls m NICHT in Sonder.Pos$Position liegt
  # UND entweder m-1 in Sonder.Pos$Position liegt! Zahleneinträge können ja länger sein
  # z.B. "2" "0" "1" "5" und ich möchte nicht jedesmal hier etwas hinzufügen, sondern
  # nur beim ersten mal. Oder  m-1 ==0, falls das erste zeichen nicht in Sonder.Pos$Position
  # liegt
  #
  # Der Rest besteht aus wunderschönen if-Abfragen für das jeweilige Symbol. Für Year gibts
  # sogar die Unterscheidung zwischen %y = 12 und %Y = 2012.

  # Insgesamt glaube ich, muss man hier noch am meisten Arbeit reinbuttern.
  v.Dateformat <- character()
  date.counter <- 0
  for(m in 1:length(v.SingleChar[[1]])){
    if(m==1){date.counter <- date.counter +1}
    if(m %in% Sonder.Pos$Position){
      v.Dateformat <- paste(v.Dateformat,Sonder.Pos$Sonder[Sonder.Pos$Position==m],
                            sep="")
      if(!(m+1) %in% Sonder.Pos$Position && m!=1){date.counter <- date.counter +1}
    } else if(!m%in%Sonder.Pos$Position &&
              (((m-1)%in%Sonder.Pos$Position) | m-1 ==0 )){
      if(colnames(Date.DF)[which(Date.DF[1,]==date.counter)]=="month"){Add <-"%m"}
      if(colnames(Date.DF)[which(Date.DF[1,]==date.counter)]=="day"){Add <-"%d"}
      if(colnames(Date.DF)[which(Date.DF[1,]==date.counter)]=="hour"){Add <-"%H"}
      if(colnames(Date.DF)[which(Date.DF[1,]==date.counter)]=="second"){Add <-"%S"}
      if(colnames(Date.DF)[which(Date.DF[1,]==date.counter)]=="minute"){Add <-"%M"}
      if(colnames(Date.DF)[which(Date.DF[1,]==date.counter)]=="year"){
        if(Date.DF[2,which(Date.DF[1,]==date.counter)]==2){Add <-"%y"}
        else if(Date.DF[2,which(Date.DF[1,]==date.counter)]==4){Add <-"%Y"}
      }
      v.Dateformat  <- paste(v.Dateformat,Add,sep="")
      if(date.counter==length(Date.DF[1,])){break}
    }
  }
  return(v.Dateformat)
# Testfunktion: f.GuessDate_for_GetWUFIplus(l)
#   j <- rep(paste("01","-","01","-","1991","-",sep=""),24)
#   h <- sprintf("%02d",0:23)
#   l <- paste (j,h, sep="")
#   l[25] <- "02-01-1991-00"

}
