---
date: 2024-01-20 17:35:56.069444-07:00
description: 'How to: .'
lastmod: '2024-03-13T22:44:54.154901-06:00'
model: gpt-4-1106-preview
summary: .
title: Datum in einen String umwandeln
weight: 28
---

## How to:
```Arduino
#include <RTClib.h>
#include <Wire.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }
  if (rtc.lostPower()) {
    Serial.println("RTC lost power, let's set the time!");
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();
  char dateStr[20];
  snprintf(dateStr, sizeof(dateStr), "%02d/%02d/%04d %02d:%02d:%02d", now.day(), now.month(), now.year(), now.hour(), now.minute(), now.second());
  Serial.println(dateStr);
  delay(1000);
}
```
Beispielausgabe:
```
05/03/2023 15:26:10
```

## Deep Dive
Umwandlung von Datums- und Zeitinformationen in Strings wird seit den Anfängen der Computerprogrammierung eingesetzt. Früher wurden spezielle Formate und Funktionen wie `strftime` in C verwendet. In Arduino-Umgebungen haben wir Bibliotheken wie `RTClib` zur Handhabung von Echtzeituhren (RTC), die es einfach machen, mit Datum und Zeit umzugehen.

Alternativ gibt es auch andere Bibliotheken wie `Time.h`, die ähnliche Funktionen bieten. Die Implementierung ist meist ähnlich: Zuerst liest man Daten aus dem RTC aus und verwendet dann eine Formatierungsfunktion oder einen Algorithmus, um das Datum in einen String zu konvertieren. Wichtig ist der richtige Einsatz von Pufferspeicher und Formaten, um korrekte Ergebnisse zu erhalten.

## See Also
- [RTClib GitHub Repository](https://github.com/adafruit/RTClib)
- [Arduino Time Library](https://www.pjrc.com/teensy/td_libs_Time.html)
- [strftime C++ Reference](http://www.cplusplus.com/reference/ctime/strftime/)
