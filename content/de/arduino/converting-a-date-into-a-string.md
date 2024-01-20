---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Umwandlung von Datumsangaben in Zeichenketten (Strings) ermöglicht die einfache Darstellung und Manipulation von Datumswerten. Es ist besonders hilfreich für die Darstellung von Daten im gewünschten Format oder zur Serialisierung für Speicher- und Netzwerkoperationen.

## So geht's:
Hier ist ein einfaches Code-Beispiel, wie man ein Datum in einen String umwandelt:

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(14, 30, 0, 1, 1, 2021);  
}

void loop() {
  time_t t = now();
  String dateStr = String(day(t)) + "-" + String(month(t)) + "-" + String(year(t));
  Serial.println(dateStr);
  delay(1000);
}
```

Wenn Sie diesen Code ausführen, erhalten Sie eine Ausgabe wie diese:

```Arduino
1-1-2021
```

## Vertiefung
Historisch gesehen ist die Verarbeitung von Datum und Uhrzeit in der Programmierung immer eine Herausforderung gewesen und kann komplex sein. Date-String-Konversion durch Arduino-Libraries wie TimeLib macht es jedoch einfacher und intuitiver.

Es gibt Alternativen zur TimeLib-Bibliothek wie die RTClib, die auch die Umwandlung von Datum und Uhrzeit in Zeichenketten unterstützt, aber die Wahl hängt von den spezifischen Anforderungen Ihrers Projekts ab.

Die TimeLib implementiert die Umwandlung von Datumsangaben in Zeichenketten, indem es die Datums- und Zeitelemente einzeln in Zeichenketten umwandelt und diese dann zu einer einzigen Zeichenkette zusammenfügt. 

## Siehe auch
[TimeLib Bibliothek](https://github.com/PaulStoffregen/Time)

[Joda-Time - Java-Datums- und Zeitbibliothek](https://www.joda.org/joda-time/)

[Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)