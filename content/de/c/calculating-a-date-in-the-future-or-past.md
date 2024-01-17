---
title:                "Ein Datum in der Zukunft oder Vergangenheit berechnen"
html_title:           "C: Ein Datum in der Zukunft oder Vergangenheit berechnen"
simple_title:         "Ein Datum in der Zukunft oder Vergangenheit berechnen"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Berechnen eines Datums in der Vergangenheit oder Zukunft ist ein häufiges Szenario in der Programmierung. Programmierer nutzen diese Funktion, um beispielsweise das Ablaufdatum einer Garantie zu bestimmen oder um Datumsbezüge in einer Anwendung zu erstellen.

# How to:
Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die Funktion "time.h" in C nutzen. Hier ist ein Beispiel, um das Datum von morgen zu berechnen:

```
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define ONE_DAY 86400 // Anzahl der Sekunden in einem Tag

int main()
{
    time_t now = time(NULL); // Aktuelles Datum und Uhrzeit abrufen
    time_t tomorrow = now + ONE_DAY; // Eine Sekunde zum aktuellen Datum und Uhrzeit addieren
    struct tm *date = localtime(&tomorrow); // In eine lokale Zeitstruktur konvertieren
    printf("Tomorrow's date is: %d/%d/%d\n", date->tm_mon + 1, date->tm_mday, date->tm_year + 1900); // Das berechnete Datum ausgeben
    return 0;
}
```
**Ausgabe:**
Tomorrow's date is: 3/2/2020

Das obige Beispiel zeigt, wie wir mithilfe der Funktionen "time.h" und "localtime" ein Datum in der Zukunft berechnen und ausgeben können. Der Code kann auch angepasst werden, um ein Datum in der Vergangenheit zu berechnen.

# Deep Dive:
Das Konzept, ein Datum in der Zukunft oder Vergangenheit zu berechnen, geht zurück auf die Entstehung der ersten kalenderbasierten Gesellschaften. Menschen haben schon immer versucht, die Zeit und das Datum zu messen und zu organisieren. Heutzutage nutzt die moderne Technologie komplexe mathematische Algorithmen, um das Berechnen eines Datums in der Zukunft oder Vergangenheit zu erleichtern.

Neben der Verwendung der "time.h" Funktion gibt es auch andere Möglichkeiten, um Datumsberechnungen in C durchzuführen. Eine alternative Methode ist die Nutzung von Datetime-Bibliotheken wie "Libdatetime" oder "Libical". Diese erweiterten Libraries bieten zusätzliche Funktionen und Methoden, um den Berechnungsprozess zu optimieren.

Bei der Implementierung der Datumsberechnung in C ist es wichtig, auf die Datentypen und Formatierung zu achten. Die Funktion "localtime" gibt beispielsweise eine lokale Zeitstruktur zurück, während "gmtime" eine GMT-Struktur zurückgibt. Durch die richtige Verwendung der Datentypen können Fehler bei der Berechnung vermieden werden.

# See Also:
- [C time.h reference](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Libdatetime Library](https://docs.python.org/2/library/datetime.html)
- [Libical Library](https://libical.github.io/libical/)