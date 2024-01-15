---
title:                "Ein Datum in der Zukunft oder Vergangenheit berechnen"
html_title:           "Arduino: Ein Datum in der Zukunft oder Vergangenheit berechnen"
simple_title:         "Ein Datum in der Zukunft oder Vergangenheit berechnen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Die Berechnung von zukünftigen und vergangenen Datum kann für verschiedene Anwendungen nützlich sein, wie z.B. für die Steuerung von Zeitabläufen, für zeitgesteuerte Aufgaben oder für die Darstellung von Datums- und Zeitanzeigen. Mit der aktuellen Version von Arduino können diese Berechnungen einfach und effektiv durchgeführt werden.

## Wie geht's

In der Arduino-Programmiersprache gibt es verschiedene Funktionen, die es ermöglichen, ein zukünftiges oder vergangenes Datum zu berechnen. Im Folgenden werden wir uns auf die Funktion "getDate" konzentrieren, die das aktuelle Datum als Rückgabewert liefert.

```Arduino
#include <Time.h> // Einbinden der Time-Library

void setup(){
    TimeElements futureDate; // Erstellt ein Objekt für das zukünftige Datum
    getDate(futureDate); // Holt sich das aktuelle Datum

    futureDate.Year += 1; // Addiert ein Jahr zu dem aktuellen Datum
    convertElements(futureDate); // Konvertiert das Datum in ein neues Format
    Serial.println(UTCDateTime(futureDate)); // Gibt das berechnete Datum aus
}
```

Die Funktion "getDate" ist Teil der Time-Library und wird verwendet, um das aktuelle Datum zu erhalten. Das zurückgegebene Datum wird in Form einer Zeitstruktur gespeichert, die verschiedene Elemente wie Jahr, Monat, Tag, Stunde, Minute und Sekunde enthält. In unserem Beispiel haben wir dem aktuellen Datum ein Jahr hinzugefügt und es in das Format UTCDateTime konvertiert, um es auf dem seriellen Monitor anzuzeigen. Natürlich kann die Funktion "getDate" auch mit anderen Rechenoperationen kombiniert werden, um komplexere Datumsberechnungen durchzuführen.

## Tiefer Einblick

Die Funktion "getDate" basiert auf dem internen Systemtimer von Arduino, der die aktuelle Zeit seit dem Einschalten des Boards zählt. Dadurch können auch Langzeitberechnungen durchgeführt werden, da das Systemdatum nicht verloren geht, selbst wenn das Board ausgeschaltet wird.

Zusätzlich zu "getDate" gibt es noch weitere hilfreiche Funktionen in der Time-Library, wie z.B. "makeTime", mit der man eine vordefinierte Zeitstruktur erstellen kann, sowie verschiedene Konvertierungsfunktionen, um das Datum in verschiedene Formate zu bringen.

## Siehe auch

- offizielle Arduino-Dokumentation zu Time-Library: https://www.arduino.cc/en/Reference/Time
- Tutorial für Zeit- und Datumsberechnungen mit Arduino: https://www.circuitbasics.com/how-to-work-with-time-and-dates-in-arduino/
- Beispielprojekt für eine Uhr mit Datumsanzeige: https://create.arduino.cc/projecthub/ansh6kevv/usdgraph-a-date-time-based-analog-clock-using-arduino-1f877c