#' Changes the DateColumn from WUFI+ Output Files to a usable Format IF it is provided in percentages (Dont ask how this happened)
#'
#' @param DateColumn DateColumn of WUFI+ Output files.
#'
#' @return Date. DateCol of WUFIPlus in correct format
#' @export Datevector A suitable formatted DateColumn for a timeseries.
#'
#' @examples
#' x <- 1
f.ConvertPercentageDate <- function(DateColumn){
  PointColumn          <- gsub(",", "\\.", DateColumn)
  # testvar == false: Kein %-Stunden Format
  #
  # testvar == true : falls irgendein Wert der letzten beiden Zeichen > 60 ist (%Stunden >60)
  #                 : Falls irgendein Wert der letzten beiden Zeichen < 1 UND  (6 Minuten Zeitschritte bewegen sich von 0, 0.1, 0.2,..,1)
  #                   falls irgendein Wert der letzten beiden Zeichen > 0!     (6 Minuten Zeitschritte bewegen sich von 0, 0.1, 0.2,..,1)
  testvar <- (any((as.numeric(substrRight(PointColumn ,2))>60)) ||
                #(any(as.numeric(substrRight(PointColumn ,2))<1) && any(as.numeric(substrRight(PointColumn ,2))>0)))
                any(findInterval(as.numeric(substrRight(PointColumn ,2)),c(0.00001,0.999999))==1))
  # Falls testvar == FALSE
  if(!testvar){
    return(DateColumn)
  }
  # Falls testvar == TRUE, also %stunden Format vorliegt.
  if(testvar){
    # Zuerst brauch ich die Länge der einzelnen Zeilen,und den häufigsten Eintrag
    # Das liegt, wie bereits oben gezeigt, daran, dass die Zeilen unterschiedlich lang sind:
    # Z.b.  1.06 Uhr -> 1.1
    #       1.00 Uhr -> 1
    #       1.15 Uhr -> 1.25
    # Dann suche ich mir die häufigste Länge mithilfe von table und names(table)
    # und definiere sie als "standartlength" und gebe die Unterschiede zu den
    # anderen Längen aus (Differences)
    lengths              <- nchar(PointColumn)
    table.entries        <- table(lengths)
    t.levels             <- as.numeric(names(table.entries))
    if(length(table.entries)== 2 &&(
      table.entries[1] == table.entries[2] ||
      table.entries[1] == table.entries[2] -1 ||
      table.entries[1] == table.entries[2] +1
      )
    ){
      standartlength     <- max(t.levels)
    } else {
      standartlength     <- t.levels[which.max(table.entries)]
    }
    Differences          <- abs(t.levels-standartlength)
    #
    hourvector           <- c()
    SaveIndize           <- c()
    # Erstellen des hourvectors, der die Stunde des Tages beeinhaltet
    # Da das Format etwas unregelmäßig ist  4.00 Uhr -> 4
    #                                       4.30 Uhr -> 4.5
    #                                       4.15 Uhr -> 4.25
    # Muss ich alternieren mit den Anzahl der Zeichen, die ich rechts abschneide
    # Hier unterscheide ich Variabel durch indize = abstand zu Standartlength
    SaveIndize <- vapply(PointColumn, FUN.VALUE = NA_real_,FUN = function(x){
      as.numeric(which((standartlength-Differences)==nchar(x)))
    })

    hourvector <- as.numeric(mapply(function(x,y){
      prevector  <- substrRight(x, (5-Differences[y]))
      if(grepl(":", prevector)){
        as.numeric(substrRight(prevector, (nchar(prevector)-1)))
      } else if(!grepl(":", prevector)){
       as.numeric(prevector)
      }

    }, x = PointColumn, y = SaveIndize))

    # for(l in 1:length(PointColumn)){
    #   indize              <- which((standartlength-Differences)==nchar(PointColumn[l]))
    #   prevector           <- substrRight(PointColumn[l],(5-Differences[indize]))
    #   # Falls nur ganze Stunden angegeben werden, ist 4 von rechts zu viel, das ":" wird
    #   # mit "ausgeschnitten" -> if-Abfrage
    #   if(grepl(":", prevector)){
    #     hourvector[l]     <- as.numeric(substrRight(prevector, (nchar(prevector)-1)))
    #   } else if(!grepl(":", prevector)){
    #     hourvector[l]     <- as.numeric(prevector)
    #     }
    #   SaveIndize[l]       <- indize
    #
    #   # Vorläufige Schleife, die bei großen Datenmengen anzeigen kann, wie weit
    #   # diese Schleife ist
    #   if(length(PointColumn) > 17600){
    #     if(l%%floor(length(PointColumn)/20)==0){
    #       print(paste(ceiling(l/length(PointColumn)*100),"% of hourvector done"))
    #     }
    #   }
    # }
    # OneHour sucht mir die erste Stelle des Vector, an dem eine Stunde (hourvector[1]+1)
    # seit Beginn vergangen ist! Die Anzahl der Schritte ist OneHour-1:
    # Beispiel:
    # hourvector <- c(1, 1.25, 1.5, 1.75, 2, ...)
    # OneHour hier ist 5, Die Anzahl der Zeitschritte zwischen zwei Stunden ist 4
    OneHour <- which((hourvector[1]+1)==hourvector)[1]
    Steps <- OneHour - 1
    # Falls es mehr als 60 Zeitschritte pro Stunde gibt, muss es Sekunden geben
    # Die SekundenSchritte sind dann 60/(Anzahl der Schritte pro Sekunden)
    # Annahme: nur SekundenZeitschritte!, Falls zusätzlich noch Minuten/Stunden
    # funktioniert der Algorithmus nicht!
    # Annahme: Man rechnet mit Teiler von 60, d.h. es kommt zu jeder vollen Minute
    # zu sekunde = 0!
    if(Steps/60 > 1){
      SecondSteps <- 60/(Steps/60)
      Second.Entries <- c()
      # Schleife um Vektor mit allen Sekundenwerten auszugeben. Theoretisch wäre es
      # nur nötig, eine ganze Minute auszugeben und zu wiederholen
      for(k in 0:(length(PointColumn)-1)){
        Second.Entries[k+1] <- (k*SecondSteps)%%60
      }
      # Anzahl der Minuten
      Minute.Entries <- PointColumn[Second.Entries==0]
      Minute.Count <-  which(Minute.Entries==PointColumn[OneHour])-1
      # Um StartMinute Auszugeben: Modulo 1 auf den ersten Zeitschritt, dann
      # Aufrunden der Zahl von z.B. 0.25 -> 2.5 -> 25 -> BREAK
      Nachkomma.Begin <- hourvector[1]%%1
      repeat{
        Nachkomma.Begin <- Nachkomma.Begin*10
        if(round(Nachkomma.Begin,2)%%1==0) break
      }
      # Umrechnung in Minuten, dazu Erstellen eines Vektors, der alle Minutendurchzählt
      StartMinute <- Nachkomma.Begin*60/100
      MinuteVector <- c()
      for(m in 0:(length(Minute.Entries)-1)){
        MinuteVector[m+1] <- round((StartMinute+m),0)%%60
      }
      MinPlusSek <- c()
      MinuteCounter <- 0
      # Hässliche Schleife, die Minuten mit Sekunden pastet
      for(n in 1:length(Second.Entries)){
        if(Second.Entries[n]==0){
          MinuteCounter <- MinuteCounter +1
        }
        MinPlusSek[n] <- paste(sprintf("%02d",MinuteVector[MinuteCounter]),
                               sprintf("%02d",Second.Entries[n]), sep=":")

      }
      # Die Stunden (4.5 oder 4) werden nun einfach abgerundet -> Stundenzahl
      # und mit den Minuten+Sekunden gepastet
      # (beide mit zwei Stellen -> 4.40 Uhr -> 04-40)
      FinalTime <- paste(sprintf("%02d",floor(hourvector)), MinPlusSek, sep=":")
    }
    # Falls es weniger als 60 Zeitschritte pro Stunde gibt, kann es nur Minuten geben
    # Die MinutenSchritte sind dann 60/(Anzahl der Schritte pro Stunde)
    if(Steps/60 < 1){
      # Um StartMinute Auszugeben: Modulo 1 auf den ersten Zeitschritt, dann
      # Aufrunden der Zahl von z.B. 0.25 -> 2.5 -> 25 -> BREAK
      Nachkomma.Begin <- hourvector[1]%%1
      if(round(Nachkomma.Begin,1)==round(Nachkomma.Begin,2)){
        tenpercentstart <- TRUE
      } else {
        tenpercentstart <- FALSE
        }
      MinuteSteps <- 60/Steps
      repeat{
        if(round(Nachkomma.Begin,2)%%1==0) {break}
        Nachkomma.Begin <- Nachkomma.Begin*10
      }
      if(tenpercentstart){Nachkomma.Begin <- Nachkomma.Begin*10}
      StartMinute <- round(Nachkomma.Begin*60/100)

      # ALT:
      # Abfrage, ob Der Hourvector mit ganzer Stunde beginnt (die fälschlicherweiße
      # in repeat mit 10 Multipliziert worden wäre)
      # ?Vorstellen des Break befehl?
      # if(round(hourvector[1]%%1,2)==round(hourvector[1]%%1,1)){
      #   StartMinute <- round(Nachkomma.Begin*60/10)
      # } else {
      #   StartMinute <- round(Nachkomma.Begin*60/100)
      # }


      # Schleife um Vektor mit allen Minutenwerten auszugeben. Theoretisch wäre es
      # nur nötig, eine ganze Stunde auszugeben und zu wiederholen
      Minute.Entries <- c()
      for(m in 0:(length(PointColumn)-1)){
        Minute.Entries[m+1] <- round((StartMinute+m*MinuteSteps),0)%%60
      }
      # Die Stunden (4.5 oder 4) werden nun einfach abgerundet -> Stundenzahl
      # und mit den Minuten gepastet
      # (beide mit zwei Stellen -> 4.40 Uhr -> 04-40)
      FinalTime <- paste(sprintf("%02d",floor(hourvector)), sprintf("%02d",Minute.Entries),
                         sep=":")
    }
    # Die Stunden (4.5 oder 4) werden nun einfach abgerundet -> Stundenzahl
    # und mit den Minuten gepastet (beide mit zwei Stellen -> 4.40 Uhr -> 04-40)
    # Dazu wird das restliche Datum (NonHourlyDate) erstellt (UNFINISHED!)
    # und wieder gepastet. Der Resultierende Vector is der Ausgabevektor
    NonHourlyDate <- substrLeft(PointColumn, 11)
    DateVector <- paste(NonHourlyDate, FinalTime, sep="")
    return(DateVector)
  }
}
