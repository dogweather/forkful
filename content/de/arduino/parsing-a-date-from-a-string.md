---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Parsing eines Datums aus einem String ist die Umwandlung von Text in ein Datenstruktur, die auf einen spezifischen Tag verweist. Programmierer machen dies, um komplexe Datumsmanipulationen zu erleichtern oder Informationen aus Benutzereingaben oder Dateien auszulesen.

## So geht's:

Hier ist ein einfacher Arduino-Code, der einen String in ein Datum umwandelt:

```Arduino
#include <TimeLib.h>  
#include <Time.h>
 
void setup() {
  Serial.begin(9600);
}
 
void loop() {
  String datum = "06.12.2021";
  int Tag = datum.substring(0,2).toInt();
  int Monat = datum.substring(3,5).toInt();
  int Jahr = datum.substring(6,10).toInt();
  
  tmElements_t tm;
  
  tm.Day = Tag;
  tm.Month = Monat;
  tm.Year = Jahr - 1970;  
  
  time_t t = makeTime(tm);
  
  Serial.println(day(t));
  Serial.println(month(t));
  Serial.println(year(t));
    
  delay(1000);
}
```
Mit diesem Code lesen wir das Datum als Tag, Monat und Jahr aus dem String und konvertieren es in einen `time_t` Datentyp.

## Vertiefung:

Historisch gesehen, war Parsing von Strings zu Datumsformaten in vielen Programmiersprachen eine Herausforderung aufgrund von verschiedenen Datumskonventionen und Zeitzonen. In Arduino wurde diese Aufgabe durch die `TimeLib.h` Bibliothek vereinfacht. 

Alternativ können auch andere Methoden wie sscanf() verwendet werden, die ein wenig komplexer sind und mehr Kontrolle bieten. Für das Einfügen von Datum und Zeit in einen String gibt es Funktionen wie sprintf().

Besonders bemerkenswert ist, dass die Arduino `TimeLib.h` Bibliothek das Jahr relativ zu 1970 berechnet. Daher muss beim Setzen des Jahres 1970 subtrahiert werden.

## Siehe auch:

Weitere Informationen und Beispiele können Sie in der offiziellen Arduino Time Library Dokumentation finden (https://playground.arduino.cc/Code/Time) und im Arduino Forum (http://forum.arduino.cc/), wo viele Diskussionen über dieses Thema geführt wurden. Für tiefere Einsichten in das Thema empfehlen sich auch Bücher wie "Programming Arduino: Getting Started with Sketches" von Simon Monk.