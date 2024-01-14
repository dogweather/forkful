---
title:                "C++: Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Datum in der Zukunft oder Vergangenheit kann in vielen Fällen hilfreich sein, zum Beispiel bei der Planung von Terminen oder der Verwaltung von Aufgaben. Mit C++ können wir dies auf effiziente Weise tun.

## Wie geht das?

Die Berechnung eines Datums in der Zukunft oder Vergangenheit erfordert zunächst die aktuelle Datumsangabe. Dazu können wir die in C++ eingebaute Funktion `time()` verwenden, die die Sekunden seit dem 1. Januar 1970 zurückgibt. Diese Werte können wir dann in ein `struct` umwandeln, um das Datum zu extrahieren. Zum Beispiel:

```C++
#include <iostream>
#include <ctime>

int main()
{
    // Aktuelles Datum erhalten
    time_t now = time(0);
    
    // In ein `struct` umwandeln
    tm *local_time = localtime(&now);

    // Das aktuelle Datum ausgeben
    std::cout << "Aktuelles Datum: " << local_time->tm_mday << "/" 
              << (local_time->tm_mon + 1) << "/" << (local_time->tm_year + 1900) << std::endl;
    
    return 0;
}
```

Die Ausgabe könnte wie folgt aussehen:

```
Aktuelles Datum: 19/10/2021
```

Um nun ein zukünftiges Datum zu berechnen, können wir die Funktion `mktime()` verwenden, die aus einer `tm` Struktur ein Integer-Datum erstellt. Dazu müssen wir in unserem `struct` das gewünschte Datum ändern und dann `mktime()` aufrufen. Zum Beispiel, um 7 Tage in der Zukunft zu berechnen:

```C++
// Gewünschtes Datum ändern
local_time->tm_mday += 7;

// In ein Integer-Datum umwandeln
time_t future_time = mktime(local_time);

// Ausgeben
std::cout << "Zukünftiges Datum: " << local_time->tm_mday << "/" 
          << (local_time->tm_mon + 1) << "/" << (local_time->tm_year + 1900) << std::endl;
```

Die Ausgabe wäre:

```
Zukünftiges Datum: 26/10/2021
```

Um ein Datum in der Vergangenheit zu berechnen, können wir ähnlich vorgehen, indem wir das gewünschte Datum im `struct` ändern und dann `mktime()` aufrufen. Die Ausgabe könnte dann zum Beispiel so aussehen:

```
Gewünschtes Datum: 1/1/2021
```

## Tiefer eintauchen

Es gibt weitere Möglichkeiten zur Manipulation von Datumsangaben in C++, wie zum Beispiel das Hinzufügen von Monaten oder Jahren, oder das Vergleichen von Datumsangaben. Es ist auch wichtig, die richtigen Datentypen zu verwenden, um mögliche Probleme mit Schaltjahren oder Datumsformaten zu vermeiden. Eine detaillierte Erklärung dieser Aspekte würde den Rahmen dieses Blog-Beitrags sprengen, aber es gibt zahlreiche Ressourcen im Internet, die detaillierte Erklärungen und Beispiele bieten.

## Siehe auch

- [C++ Zeit- und Datum-Funktionen](https://www.geeksforgeeks.org/c-work-time/) 
- [Richtiger Umgang mit Datum und Uhrzeit in C++](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm) 
- [C++ Datum und Uhrzeit Bibliothek](https://www.cplusplus.com/reference/ctime/)