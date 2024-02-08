---
title:                "Einen Datum aus einem String analysieren"
aliases:
- de/cpp/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:13:33.202127-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einen Datum aus einem String analysieren"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String beinhaltet die Interpretation des Stringformats, um Komponenten wie Tag, Monat und Jahr zu extrahieren. Programmierer tun dies, um Benutzereingaben zu verarbeiten, Datendateien zu lesen oder mit APIs zu interagieren, die Daten in Stringformaten kommunizieren. Es ist wesentlich für die Datenverarbeitung, Validierung und das Durchführen von Datumsarithmetik in Anwendungen.

## Wie:
In modernem C++ kann man die `<chrono>`-Bibliothek verwenden, um mit Daten und Zeiten nativ umzugehen, aber sie unterstützt nicht direkt das Parsen aus Strings ohne manuelles Parsen für komplexere Formate. Für ISO 8601-Datenformate und einfache benutzerdefinierte Formate ist hier, wie Sie das Parsen bewerkstelligen können.

**Verwendung von `<chrono>` und `<sstream>`:**
```cpp
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_str = "2023-04-15"; // ISO 8601-Format
    std::istringstream iss(date_str);
    
    std::chrono::year_month_day parsed_date;
    iss >> std::chrono::parse("%F", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Geparstes Datum: " << parsed_date << std::endl;
    } else {
        std::cout << "Datum konnte nicht geparst werden." << std::endl;
    }
    
    return 0;
}
```
Beispiel für Ausgabe:
```
Geparstes Datum: 2023-04-15
```

Für komplexere Formate oder wenn Sie mit älteren C++-Versionen arbeiten, sind Dritt-Bibliotheken wie `date.h` (Howard Hinnants Date-Bibliothek) beliebt. Hier ist, wie Sie verschiedene Formate damit parsen können:

**Verwendung der `date.h`-Bibliothek:**
Stellen Sie sicher, dass Sie die Bibliothek installiert haben. Sie finden sie [hier](https://github.com/HowardHinnant/date).

```cpp
#include "date/date.h"
#include <iostream>

int main() {
    std::string date_str = "April 15, 2023";
    
    std::istringstream iss(date_str);
    date::sys_days parsed_date;
    iss >> date::parse("%B %d, %Y", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Geparstes Datum: " << parsed_date << std::endl;
    } else {
        std::cout << "Datum konnte nicht aus dem String geparst werden." << std::endl;
    }

    return 0;
}
```
Beispiel für Ausgabe (kann je nach Systemlokalisierung und Datumeinstellungen variieren):
```
Geparstes Datum: 2023-04-15
```
