---
title:    "Arduino: Vergleich von zwei Daten"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Warum

In der Welt der Programmierung gibt es viele Situationen, in denen man mit zwei Datumsangaben arbeiten muss. Das kann die Überprüfung von Fälligkeiten, die Berechnung von Zeitintervallen oder die Erfassung von Daten in einem Kalender sein. Egal aus welchem Grund, das Vergleichen von zwei Daten ist eine wichtige Fähigkeit, die jeder Programmierer beherrschen sollte.

## Wie man zwei Daten auf Arduino vergleicht

Um zwei Daten auf dem Arduino zu vergleichen, gibt es mehrere Möglichkeiten. Eine einfache Methode ist die Verwendung der "time.h" Library, die Funktionen für die Verarbeitung von Datum und Uhrzeit bietet. Schauen wir uns ein Beispiel an, das zwei Daten vergleicht und eine Ausgabe basierend auf dem Ergebnis ausgibt:

```Arduino
#include <time.h>
time_t date1 = {2021, 01, 01, 0 ,0 ,0};
time_t date2 = {2020, 12, 31, 23, 59, 59};

if (difftime(mktime(&date1), mktime(&date2)) == 86400){
    Serial.println("Die Daten sind gleich.");
} else {
    Serial.println("Die Daten sind nicht gleich.");
}
```

Die Ausgabe dieses Beispiels wird "Die Daten sind gleich." sein, da der Unterschied zwischen den beiden Daten genau einen Tag beträgt (86400 Sekunden).

## Tiefentauchen

Wenn es um das Vergleichen von Daten auf dem Arduino geht, gibt es ein paar Dinge zu beachten. Eine Sache ist, dass der Arduino keine Batterie hat, um die Daten und Uhrzeit zu speichern. Das bedeutet, dass jedes Mal, wenn der Arduino neu gestartet wird, die Uhr auf den Standardwert zurückgesetzt wird. Für genaue Ergebnisse ist es daher wichtig, die Uhrzeit regelmäßig mit einer externen Quelle wie einem RTC-Modul zu synchronisieren.

Eine weitere Sache ist die Berücksichtigung von Schaltjahren und unterschiedlichen Längen der Monate. Die "time.h" Library berücksichtigt diese Faktoren automatisch, aber es ist wichtig, sich darüber im Klaren zu sein, wenn man eigene Methoden für das Vergleichen von Daten schreibt.

Außerdem gibt es verschiedene Funktionen in der "time.h" Library, mit denen man Daten und Uhrzeiten auf unterschiedliche Weise vergleichen kann. Es lohnt sich, sich mit diesen vertraut zu machen und zu experimentieren, um die beste Lösung für das eigene Programm zu finden.

## Siehe auch

- [time.h Library Referenz](https://www.arduino.cc/reference/en/libraries/time/)
- [RTC-Modul Tutorial](https://learn.adafruit.com/adding-a-real-time-clock-to-arduino)
- [Guide für Datum und Uhrzeit auf dem Arduino](https://www.curiousmotor.com/how-to-get-date-and-time-on-the-arduino/)