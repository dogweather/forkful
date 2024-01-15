---
title:                "Vergleich von zwei Daten"
html_title:           "Arduino: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Warum

Warum sollte man sich mit dem Vergleichen von zwei Daten beschäftigen? Ganz einfach: Datumswerte sind oft entscheidend für die Funktionalität von Anwendungen und eine korrekte Vergleichsfunktion kann dabei helfen, mögliche Fehler zu verhindern.

# Wie geht das?

Um zwei Daten miteinander zu vergleichen, gibt es verschiedene Ansätze. Im Folgenden werden zwei Beispiele mit dem aktuellen Stand der Arduino-Programmierung gezeigt.

```Arduino
#include <TimeLib.h> // Bibliothek für Zeitfunktionen

void setup() {
  // Initialisierung der seriellen Verbindung
  Serial.begin(9600); 

  // Erstellung von zwei verschiedenen Zeitvariablen
  // mit je einem festgelegten Datum
  // Beachte: bei der Angabe von Monat und Tag muss eine führende Null verwendet werden 
  // 03.02.2020 --> 03/02/20
  // 13.10.2021 --> 13/10/21
  time_t date1 = makeTime(0, 0, 0, 03, 02, 2020);
  time_t date2 = makeTime(0, 0, 0, 10, 13, 2021);

  // Vergleich der beiden Daten
  if (date1 < date2) {
    Serial.println("Date 1 liegt vor Date 2");
  } else if (date1 > date2) {
    Serial.println("Date 2 liegt vor Date 1");
  } else {
    Serial.println("Beide Daten sind identisch");
  }
}

void loop() {
  // Hier können weitere Funktionen oder Aktionen ausgeführt werden
}
```

Die Ausgabe dieses Codes wäre in der seriellen Konsole: "Date 1 liegt vor Date 2".

Ein weiterer Ansatz ist die Verwendung von UNIX-Zeitstempeln. Diese sind eine gängige Darstellung von Datum und Uhrzeit als Anzahl der seit dem 01.01.1970 vergangenen Sekunden. Mit dieser Darstellung lassen sich Daten einfach vergleichen, indem man die entsprechenden Zeitspannen berechnet und vergleicht.

```Arduino
#include <TimeLib.h> // Bibliothek für Zeitfunktionen

void setup() {
  // Initialisierung der seriellen Verbindung
  Serial.begin(9600); 

  // Erstellung von zwei Variablen als UNIX-Zeitstempel
  // hier werden die aktuellen Zeiten verwendet
  time_t now = now();
  time_t future = now() + 24*60*60; // entspricht einem Tag in Sekunden
  // Die Funktion now() gibt die aktuelle Uhrzeit als UNIX-Zeitstempel zurück

  // Vergleich der beiden Stempel
  if (now < future) {
    Serial.println("Der zukünftige Zeitstempel liegt später als der aktuelle");
  } else if (now > future) {
    Serial.println("Der zukünftige Zeitstempel liegt früher als der aktuelle");
  } else {
    Serial.println("Beide Zeitstempel sind identisch");
  }
}

void loop() {
  // Hier können weitere Funktionen oder Aktionen ausgeführt werden
}
```

Die Ausgabe dieses Codes wäre: "Der zukünftige Zeitstempel liegt später als der aktuelle", da wir mit "future" einen Zeitstempel erzeugt haben, der genau einen Tag in der Zukunft liegt.

# Tiefergehende Informationen

Das Vergleichen von Daten kann in komplexeren Anwendungen auch noch weitere Herausforderungen mit sich bringen, z.B. beim Vergleich unterschiedlicher Zeitzonen oder bei der Berücksichtigung von Schaltjahren. Hier ist es wichtig, sich in die genaue Funktionsweise und die möglichen Fallstricke einzulesen und gezielt auf Bibliotheken oder Funktionen zurückzugreifen, die diese Probleme bereits lösen.

# Siehe auch

- [Offizielle Dokumentation für die Time-Library](https://www.arduino.cc/reference/en/libraries/time/)
- [Informationen zu UNIX-Zeitstempeln](https://www.epochconverter.com/)
- [Beispielprojekt zur Berechnung von Schaltjahren mit Arduino](https://create.arduino.cc/projecthub/dancili/calculating-leap-years-with-arduino-087503)