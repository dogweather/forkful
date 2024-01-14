---
title:                "C: Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von zukünftigen oder vergangenen Datumsangaben ist eine wichtige Funktion in vielen Anwendungen. Ein Beispiel dafür ist das Erstellen von Kalendern oder Planungswerkzeugen. Durch das Verständnis dieser Fähigkeit können Sie Ihre eigenen Anwendungen effizienter und zuverlässiger gestalten.

## Wie geht es

Die Berechnung von zukünftigen oder vergangenen Daten ist dank der Datentypen und Funktionen in C relativ einfach. Zunächst müssen wir das aktuelle Datum und die Anzahl der gesuchten Tage in Variablen speichern. Dies kann mit den Datentypen `struct tm` und `time_t` erfolgen.

Danach können wir die Funktion `mktime()` verwenden, um die angegebenen Daten in ein `time_t` Objekt umzuwandeln. Das zurückgegebene Objekt enthält die Anzahl der Sekunden, die seit dem 01. Januar 1970 vergangen sind, auch bekannt als die "Unix Epoch".

Im nächsten Schritt können wir diese Anzahl an Sekunden mit der Anzahl der Sekunden in einem Tag (86400) multiplizieren und entweder addieren oder subtrahieren, je nachdem ob wir zukünftige oder vergangene Daten berechnen möchten. Das Ergebnis können wir dann wieder mit `localtime()` in eine `struct tm` umwandeln und die gewünschten Datumsinformationen abrufen.

```C
// Beispielausgabe für die Berechnung des Datums in 100 Tagen in der Zukunft
#include <stdio.h>
#include <time.h>

int main()
{
    // Initialisierung der Variablen
    struct tm currentDate;
    time_t currentTime;

    // Eingabe des aktuellen Datums
    printf("Geben Sie das aktuelle Datum ein (TT.MM.JJJJ): ");
    scanf("%d.%d.%d", &currentDate.tm_mday, &currentDate.tm_mon, &currentDate.tm_year);

    // Konvertierung des Datums in ein time_t Objekt
    currentTime = mktime(&currentDate);

    // Berechnung des Datums in 100 Tagen in der Zukunft
    currentTime += (86400 * 100);

    // Konvertierung des zukünftigen Datums in eine struct tm
    struct tm *futureDate = localtime(&currentTime);

    // Ausgabe des berechneten Datums
    printf("Das Datum in 100 Tagen in der Zukunft ist: %02d.%02d.%04d", futureDate->tm_mday, futureDate->tm_mon + 1, futureDate->tm_year + 1900);

    return 0;
}
```

## Tiefer Einblick

Obwohl das Berechnen von Datumsangaben in C relativ einfach ist, gibt es einige wichtige Dinge zu beachten. Zum einen ist es wichtig, dass die eingegebenen Daten korrekt formatiert sind, da sonst falsche Ergebnisse erzielt werden können.

Außerdem kann es durch Schaltjahre, Sommer- und Winterzeit sowie Zeitzonen zu Abweichungen in der berechneten Zeit kommen. Um diese zu vermeiden, ist es empfehlenswert, die Funktion `mktime()` mit der Funktion `gmtime()` (für die berechnete Zeit in GMT/UTC) oder `localtime()` (für die berechnete Zeit in der lokalen Zeitzone) zu kombinieren, um eine genauere Berechnung zu erreichen.

## Siehe auch

- <https://www.tutorialspoint.com/c_standard_library/time_h.htm>
- <https://www.programiz.com/c-programming/library-function/time/mktime>
- <https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm>