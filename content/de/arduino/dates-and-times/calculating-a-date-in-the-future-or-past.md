---
date: 2024-01-20 17:30:58.557754-07:00
description: "Datum-Berechnungen erlauben die Ermittlung vergangener oder zuk\xFC\
  nftiger Tage. Programmierer nutzen diese, um Zeitintervalle zu verwalten, Ereignisse\
  \ zu\u2026"
lastmod: '2024-02-25T18:49:51.208681-07:00'
model: gpt-4-1106-preview
summary: "Datum-Berechnungen erlauben die Ermittlung vergangener oder zuk\xFCnftiger\
  \ Tage. Programmierer nutzen diese, um Zeitintervalle zu verwalten, Ereignisse zu\u2026"
title: "Berechnung eines zuk\xFCnftigen oder vergangenen Datums"
---

{{< edit_this_page >}}

## Was & Warum?
Datum-Berechnungen erlauben die Ermittlung vergangener oder zukünftiger Tage. Programmierer nutzen diese, um Zeitintervalle zu verwalten, Ereignisse zu planen oder Zeitspannen zu messen.

## So geht's:
```arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("RTC nicht gefunden!");
    while (1);
  }
  
  DateTime jetzt = rtc.now();
  DateTime zukunft = jetzt + TimeSpan(30,0,0,0); // 30 Tage in die Zukunft
  DateTime vergangenheit = jetzt - TimeSpan(5,0,0,0); // 5 Tage in die Vergangenheit

  // Zukunft
  Serial.print("Zukunft: ");
  Serial.print(zukunft.day());
  Serial.print(".");
  Serial.print(zukunft.month());
  Serial.print(".");
  Serial.println(zukunft.year());

  // Vergangenheit
  Serial.print("Vergangenheit: ");
  Serial.print(vergangenheit.day());
  Serial.print(".");
  Serial.print(vergangenheit.month());
  Serial.print(".");
  Serial.println(vergangenheit.year());
}

void loop() {
  // Nichts zu tun hier
}
```
Ausgabe könnte sein:
```
Zukunft: 28.4.2023
Vergangenheit: 23.3.2023
```

## Tiefere Einblicke
In der Frühzeit der Programmierung waren Datum-Berechnungen kompliziert, man brauchte Algorithmen für jeden Kalendertyp. Heute vereinfachen Bibliotheken wie `RTClib` das Prozedere erheblich. Alternativ könnten Programmierer das Datum manuell berechnen, indem sie Sekunden zählen und Schaltjahre beachten, aber warum kompliziert, wenn es auch einfach geht? Die `RTClib`-Bibliothek benutzt Objekte, wie `DateTime` und `TimeSpan`, um Zeiträume zu repräsentieren und bequem zu handhaben.

## Siehe auch
- Die RTClib Dokumentation für weitere Funktionen: https://github.com/adafruit/RTClib
- Der Arduino Time Library für alternative Methoden: https://www.arduino.cc/en/Reference/Time
