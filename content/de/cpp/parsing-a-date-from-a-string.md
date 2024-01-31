---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:35:08.019179-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen eines Datums aus einem String wandelt Text in ein Datum um, das der Computer verarbeiten kann. Programmierer brauchen das, um Daten zu vergleichen, zu sortieren oder Zeitabhängigkeiten zu managen.

## So geht's:

Hier ist ein einfaches Beispiel, wie du mit C++ ein Datum aus einem String parst:

```C++
#include <iostream>
#include <sstream>
#include <iomanip>
#include <ctime>

int main() {
    std::string dateString = "2023-03-22";
    std::tm tm = {};
    std::istringstream ss(dateString);

    ss >> std::get_time(&tm, "%Y-%m-%d"); // Strenge Formatierung
    if (ss.fail()) {
        std::cerr << "Parse-Fehler!" << std::endl;
        return 1;
    }

    // Hier könntest du mit tm weiterarbeiten
    std::cout << "Erfolg! Datum: "
              << std::put_time(&tm, "%d.%m.%Y") << std::endl;

    return 0;
}
```

Ausgabe:
```
Erfolg! Datum: 22.03.2023
```

## Deep Dive:

Das Parsen von Daten wurde wichtig mit dem Aufkommen von digitaler Kommunikation. Früher wurde es manuell erledigt – ein mühsamer Prozess. Heutzutage gibt es in vielen Programmiersprachen eingebaute Möglichkeiten das zu tun, in C++ etwa mit `get_time` und `put_time`.

Es gibt Alternativen wie die `strptime()`-Funktion aus der C Standardbibliothek oder Bibliotheken von Drittanbietern wie `date.h` von Howard Hinnant, welche mehr Funktionalitäten bieten.

Hinsichtlich der Implementierung sollte man achten, dass verschiedene Länder unterschiedliche Datumsformate nutzen und Zeitzonen sowie Sommerzeit berücksichtigt werden sollten. Fehlerbehandlung, also das Erkennen und richtige Reagieren auf ungültige Daten, ist ebenso von Bedeutung.

## Siehe auch:

- C++ Date and Time [Tutorial](http://www.cplusplus.com/reference/ctime/)
- Howard Hinnant's Date library [GitHub Repository](https://github.com/HowardHinnant/date)
- ISO 8601 Date and Time Formats [Information](https://en.wikipedia.org/wiki/ISO_8601)
