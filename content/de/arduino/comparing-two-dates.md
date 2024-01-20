---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?

Zwei Daten zu vergleichen bedeutet, festzustellen, welches Datum vor oder nach dem anderen kommt oder ob sie gleich sind. Programmierer machen das, um Ereignisse zu sequenzieren und Zeitabläufe zu verwalten.

## Wie macht man das:

Hier ist ein einfacher Code-Beispiel, wie Sie zwei Daten in Arduino vergleichen können:

```Arduino
#include <TimeLib.h>

time_t t1, t2;

void setup() {
  Serial.begin(9600);
  setTime(8, 29, 0, 1, 1, 2022); // setzen Sie die aktuelle Zeit
  t1 = now();  // Speichern Sie die aktuelle Zeit
  delay(2000); // Warten Sie für 2 Sekunden
  t2 = now();  // Speichern Sie die neue aktuelle Zeit
}

void loop() {
  if (t1 < t2){
    Serial.print("t1 ist vor t2");
  } else if (t1 > t2){
    Serial.print("t1 ist nach t2");
  } else {
    Serial.print("t1 und t2 sind gleich");
  }
}
```
In der Seriellen Monitor sollte nun "t1 ist vor t2" ausgegeben werden.

## Deep Dive:

Diese Methode des Datenvergleichs hat eine lange Geschichte in der Computerprogrammierung und wurde durch die Notwendigkeit zur Erfassung und Verwaltung von Zeitinformationen in Computersystemen gefördert.

Alternativ könnten Sie die DateTime Bibliothek verwenden, die etwas mehr Flexibilität bietet. Die Grundprinzipien bleiben jedoch unverändert.

Etwas, das beachtet werden muss, ist die Möglichkeit von Seiteneffekten bei gleichzeitiger Änderung von Daten während des Vergleichsvorgangs. Deshalb ist eine gute Praxis, immer Kopien der Daten zu vergleichen.

## Siehe auch:

- [Arduino Time Library](https://www.arduino.cc/reference/en/libraries/time/)
- [Gute Codierungspraktiken](https://www.arduino.cc/en/Tutorial/Foundations)