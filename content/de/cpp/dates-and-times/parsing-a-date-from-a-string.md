---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:33.202127-07:00
description: "Wie: In modernem C++ kann man die `<chrono>`-Bibliothek verwenden, um\
  \ mit Daten und Zeiten nativ umzugehen, aber sie unterst\xFCtzt nicht direkt das\
  \ Parsen\u2026"
lastmod: '2024-03-13T22:44:54.194202-06:00'
model: gpt-4-0125-preview
summary: "In modernem C++ kann man die `<chrono>`-Bibliothek verwenden, um mit Daten\
  \ und Zeiten nativ umzugehen, aber sie unterst\xFCtzt nicht direkt das Parsen aus\
  \ Strings ohne manuelles Parsen f\xFCr komplexere Formate."
title: Einen Datum aus einem String analysieren
weight: 30
---

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
