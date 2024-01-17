---
title:                "Das aktuelle Datum erhalten"
html_title:           "Arduino: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Aktuelles Datum mit Arduino bekommen

## Was & Warum?
Das aktuelle Datum bekommen ist ein wichtiger Bestandteil der Programmierung. Es ermöglicht es uns, aktuelle Zeitstempel zu setzen oder zu überprüfen, ob ein bestimmtes Datum bereits erreicht wurde. Dies ist besonders nützlich für Projekte, die eine zeitliche Komponente haben, wie z.B. automatisierte Anlagensteuerungen oder Datensammlungen.

## Wie geht das?
Um das aktuelle Datum mit Arduino zu bekommen, können wir die ```millis()``` Funktion verwenden. Diese Funktion gibt die vergangenen Millisekunden seit dem Start des Programms zurück. Um diese in ein Datum umzuwandeln, nehmen wir einfach den aktuellen Tag und berechnen die vergangenen Tage seit dem 1. Januar 1970. Dies ist der sogenannte "Unix Epoch". Wir können dann diese Anzahl von Tagen in ein Datum umwandeln, indem wir die entsprechenden Werte für den Tag, Monat und Jahr berechnen.

```arduino
unsigned long days = millis() / 86400000; // 86400000 Millisekunden entsprechen einem Tag
int year = 1970; // Unix Epoch Jahr
int month, day;

// Berechne den Monat und Tag basierend auf der Anzahl von vergangenen Tagen
for (int i = 0; i < days; i++) {
  if (year % 4 == 0) { // Schaltjahre berücksichtigen
    if (days > 366) {
      days -= 366;
      year++;
    }
  } else {
    if (days > 365) {
      days -= 365;
      year++;
    }
  }
}

// Berechne den Monat basierend auf der Anzahl von verbleibenden Tagen
for (int i = 1; i <= 12; i++) {
  int daysInMonth = 31;
  if (i == 4 || i == 6 || i == 9 || i == 11) {
    daysInMonth = 30;
  }
  if (i == 2) {
    if (year % 4 == 0) { // Schaltjahr
      daysInMonth = 29;
    } else {
      daysInMonth = 28;
    }
  }
  if (days > daysInMonth) {
    days -= daysInMonth;
    month++;
  }
}

day = days;

// Ausgabe des Datums
Serial.print(day);
Serial.print("/");
Serial.print(month);
Serial.print("/");
Serial.println(year);

```

###Beispiel Ausgabe
``` 
28/10/2021
```

## Tief eintauchen
Die Idee, das Datum basierend auf der Anzahl von vergangenen Tagen seit dem Unix Epoch zu berechnen, stammt aus dem sogenannten "Unix Time Stamp". Dies ist ein in der UNIX-Welt weit verbreitetes Zeitformat, das auch von vielen anderen Programmiersprachen und Betriebssystemen übernommen wurde.

Eine alternative Methode, das Datum mit Arduino zu bekommen, ist die Verwendung eines speziellen Real-Time-Clocks (RTC) Moduls. Diese Module werden an den Arduino angeschlossen und enthalten eine batteriebetriebene Uhr, die unabhängig vom Arduino funktioniert und somit genaue Zeit- und Datumsinformationen liefern kann.

Unabhängig von der verwendeten Methode ist es wichtig zu beachten, dass die Genauigkeit des Datumswerts vom genutzten Mikrocontroller abhängen kann. In manchen Fällen kann es notwendig sein, die Zeit mit einer externen Quelle, wie z.B. einem GPS-Modul, synchronisieren zu lassen.

## Weitere Ressourcen
- [Unix Time Stamp](https://en.wikipedia.org/wiki/Unix_time)
- [Tutorial zur Verwendung von RTC Modulen mit Arduino](https://www.electronics-lab.com/project/using-the-ds1307-real-time-clock-with-arduino/)
- [Tutorial zur Verwendung von GPS-Modulen mit Arduino](https://learn.sparkfun.com/tutorials/gps-basics/all)