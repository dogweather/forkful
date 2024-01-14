---
title:    "Arduino: Das aktuelle Datum erhalten"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum
In diesem Beitrag geht es um die Programmierung von Arduino und warum es wichtig ist, das aktuelle Datum abzurufen. Das aktuelle Datum kann in vielen Projekten von entscheidender Bedeutung sein, beispielsweise in einer Wetterstation oder einer automatisierten Bewässerungsanlage für den Garten.

## Wie
Die aktuelle Datumserfassung kann mit der Funktion `millis()` in Arduino erreicht werden. Diese Funktion gibt die Anzahl der Millisekunden seit dem Start des Arduino-Boards zurück. Mit dieser Information können wir das aktuelle Datum und die Uhrzeit berechnen.

```Arduino
unsigned long currentTime = millis(); // aktuelle Zeit in Millisekunden
int seconds = (currentTime / 1000) % 60; // Sekunden berechnen
int minutes = (currentTime / (1000 * 60)) % 60; // Minuten berechnen
int hours = (currentTime / (1000 * 60 * 60)) % 24; // Stunden berechnen
```

Um das aktuelle Datum abzurufen, müssen wir auch den Tag, den Monat und das Jahr berücksichtigen. Hier können wir die Funktionen `day()`, `month()` und `year()` verwenden.

```Arduino
int currentDay = day(); // aktuellen Tag abrufen
int currentMonth = month(); // aktuellen Monat abrufen
int currentYear = year(); // aktuelles Jahr abrufen
```

Mit diesen Informationen können wir das aktuelle Datum in einem bestimmten Format anzeigen. Hier ist ein Beispiel, wie man das aktuelle Datum im Format `TT.MM.JJJJ` ausgeben kann.

```Arduino
Serial.print(currentDay); // Tag ausgeben
Serial.print("."); // Punkt hinzufügen
Serial.print(currentMonth); // Monat ausgeben
Serial.print("."); // Punkt hinzufügen
Serial.println(currentYear); // Jahr ausgeben
```

Die Ausgabe wird dann wie folgt aussehen: 12.05.2021. Natürlich können Sie das Format an Ihre Bedürfnisse anpassen.

## Deep Dive
Es gibt auch die Möglichkeit, das aktuelle Datum über eine RTC (Real-Time Clock) zu erhalten. Eine RTC ist eine elektronische Uhr, die eine unabhängige Stromquelle hat und somit auch weiterläuft, wenn der Arduino ausgeschaltet ist.

Hier ist ein Beispiel, wie Sie das aktuelle Datum von einer RTC abrufen können:

```Arduino
#include <Wire.h>
#include "RTClib.h"
RTC_DS1307 rtc;

void setup() {
  Wire.begin();
  rtc.begin();
  // Uhrzeit und Datum werden hier eingestellt
  // rtc.adjust(DateTime(DATE, TIME));
}
void loop() {
  DateTime now = rtc.now(); // aktuelles Datum und Uhrzeit abrufen
  int currentDay = now.day(); // aktuellen Tag abrufen
  // Weitere Funktionen wie in "Wie" erklärt benutzen
}
```

Es ist wichtig zu beachten, dass Sie die Uhrzeit und das Datum der RTC einmal einstellen müssen, bevor Sie das aktuelle Datum abrufen können.

## Siehe auch
- [Arduino - millis()](https://www.arduino.cc/reference/en/language/functions/time/millis/)
- [Arduino - day()](https://www.arduino.cc/reference/en/language/variables/time/day/)
- [RTClib - Library für RTCs](https://github.com/adafruit/RTClib)