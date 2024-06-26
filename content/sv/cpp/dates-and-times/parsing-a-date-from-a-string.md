---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:39.512332-07:00
description: "Hur man g\xF6r: I modern C++ kan du anv\xE4nda biblioteket `<chrono>`\
  \ f\xF6r att hantera datum och tider p\xE5 ett inbyggt s\xE4tt, men det st\xF6der\
  \ inte direkt tolkning\u2026"
lastmod: '2024-03-13T22:44:38.218558-06:00'
model: gpt-4-0125-preview
summary: "I modern C++ kan du anv\xE4nda biblioteket `<chrono>` f\xF6r att hantera\
  \ datum och tider p\xE5 ett inbyggt s\xE4tt, men det st\xF6der inte direkt tolkning\
  \ fr\xE5n str\xE4ngar utan manuell bearbetning f\xF6r mer komplexa format."
title: "Analysera ett datum fr\xE5n en str\xE4ng"
weight: 30
---

## Hur man gör:
I modern C++ kan du använda biblioteket `<chrono>` för att hantera datum och tider på ett inbyggt sätt, men det stöder inte direkt tolkning från strängar utan manuell bearbetning för mer komplexa format. Dock, för ISO 8601-datumsformat och enkla anpassade format, här är hur du kan åstadkomma tolkningen.

**Använda `<chrono>` och `<sstream>`:**
```cpp
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_str = "2023-04-15"; // ISO 8601-format
    std::istringstream iss(date_str);
    
    std::chrono::year_month_day parsed_date;
    iss >> std::chrono::parse("%F", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Tolkat datum: " << parsed_date << std::endl;
    } else {
        std::cout << "Misslyckades med att tolka datum." << std::endl;
    }
    
    return 0;
}
```
Exempelutdata:
```
Tolkat datum: 2023-04-15
```

För mer komplexa format eller när man hanterar äldre versioner av C++, är tredjepartsbibliotek som `date.h` (Howard Hinnants datum-bibliotek) populärt. Så här kan du tolka olika format med det:

**Använda `date.h`-biblioteket:**
Se till att du har installerat biblioteket. Du kan hitta det [här](https://github.com/HowardHinnant/date).

```cpp
#include "date/date.h"
#include <iostream>

int main() {
    std::string date_str = "April 15, 2023";
    
    std::istringstream iss(date_str);
    date::sys_days parsed_date;
    iss >> date::parse("%B %d, %Y", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Tolkat datum: " << parsed_date << std::endl;
    } else {
        std::cout << "Misslyckades med att tolka datum från strängen." << std::endl;
    }

    return 0;
}
```
Exempelutdata (kan variera beroende på ditt systems lokala inställningar och datuminställningar):
```
Tolkat datum: 2023-04-15
```
