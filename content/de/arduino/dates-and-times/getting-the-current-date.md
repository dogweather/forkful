---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:48.037680-07:00
description: "Das aktuelle Datum in Arduino-Projekten zu bekommen, beinhaltet das\
  \ Abrufen von Echtzeitinformationen, die f\xFCr das Logging, Zeitstempeln oder das\
  \ Planen\u2026"
lastmod: 2024-02-19 22:05:13.088317
model: gpt-4-0125-preview
summary: "Das aktuelle Datum in Arduino-Projekten zu bekommen, beinhaltet das Abrufen\
  \ von Echtzeitinformationen, die f\xFCr das Logging, Zeitstempeln oder das Planen\u2026"
title: Den aktuellen Datum abrufen
---

{{< edit_this_page >}}

## Was & Warum?
Das aktuelle Datum in Arduino-Projekten zu bekommen, beinhaltet das Abrufen von Echtzeitinformationen, die für das Logging, Zeitstempeln oder das Planen von Aufgaben entscheidend sein können. Programmierer benötigen diese Fähigkeit oft, um die Funktionalität zu verbessern, die Datenrelevanz zu sichern und zeitkritische Operationen in ihren IoT- und Embedded-Projekten zu erleichtern.

## Wie geht das:
Arduino selbst hat keine eingebaute Methode, um direkt das aktuelle Datum zu fetchen, da ihm eine Echtzeituhr (RTC) fehlt. Dies kann jedoch mit externen RTC-Modulen wie dem DS3231 und Bibliotheken wie `RTClib`, entwickelt von Adafruit, erreicht werden, was die Schnittstelle mit diesen Modulen unkompliziert macht.

Stellen Sie zunächst sicher, dass die `RTClib` Bibliothek in Ihrer Arduino IDE installiert ist. Verbinden Sie dann Ihr RTC-Modul gemäß seiner Dokumentation mit Ihrem Arduino.

Hier ist ein einfaches Beispiel, um Ihnen den Einstieg zu erleichtern:

```cpp
#include <Wire.h>
#include "RTClib.h"

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println("Konnte RTC nicht finden");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC hat Strom verloren, lass uns die Zeit einstellen!");
    // Wenn die Zeit auf einem neuen Gerät eingestellt werden muss oder nach einem Stromausfall, können Sie das hier tun.
    // rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  Serial.print("Aktuelles Datum: ");
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);

  delay(3000); // Verzögerung um 3 Sekunden, um den Serial-Spam zu reduzieren
}
```

Beispielausgabe (unter der Annahme, dass Ihre RTC zuvor eingestellt wurde):

```
Aktuelles Datum: 2023/4/15
```

Dieser Code initialisiert das RTC-Modul und ruft dann in der Schleife alle 3 Sekunden das aktuelle Datum ab und druckt es auf den Serial Monitor. Denken Sie daran, die `rtc.adjust(...)` Zeile kann auskommentiert und modifiziert werden, um das Datum und die Zeit der RTC anfänglich oder nach einem Stromverlust einzustellen.
