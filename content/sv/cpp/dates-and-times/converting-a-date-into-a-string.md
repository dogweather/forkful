---
date: 2024-01-20 17:36:34.895498-07:00
description: "How to: (Hur man g\xF6r:) I \xE4ldre C++ versioner, var konvertering\
  \ av datum en ganska besv\xE4rlig uppgift med `strftime` och `time_t`. Med C++11\
  \ introducerades\u2026"
lastmod: '2024-04-05T22:50:52.532343-06:00'
model: gpt-4-1106-preview
summary: "(Hur man g\xF6r:) I \xE4ldre C++ versioner, var konvertering av datum en\
  \ ganska besv\xE4rlig uppgift med `strftime` och `time_t`."
title: "Omvandla ett datum till en str\xE4ng"
weight: 28
---

## How to: (Hur man gör:)
```C++
#include <iostream>
#include <chrono>
#include <iomanip>

int main() {
    // Få nuvarande datum och tid
    auto now = std::chrono::system_clock::now();
    // Konvertera till ett tm objekt
    std::time_t t_now = std::chrono::system_clock::to_time_t(now);
    std::tm* date_ptr = std::localtime(&t_now);

    // Format och konvertera till sträng
    std::stringstream ss;
    ss << std::put_time(date_ptr, "%Y-%m-%d %H:%M:%S");
    
    // Skriv ut strängen
    std::string date_str = ss.str();
    std::cout << "Datum som sträng: " << date_str << std::endl;
}

```
Sample output:
```
Datum som sträng: 2023-04-05 15:45:12
```

## Deep Dive (Djupdykning)
I äldre C++ versioner, var konvertering av datum en ganska besvärlig uppgift med `strftime` och `time_t`. Med C++11 introducerades `<chrono>` biblioteket som gav en mer robust hantering av tid och datum. `std::put_time` är en modernare funktion som finns i `<iomanip>`, vilket underlättar formatering av datum och tid.

Alternativ till `std::put_time` inkluderar att använda tredjepartsbibliotek som Boost's DateTime eller strängmanipuleringsfunktioner för att skapa egna datumsträngformat.

Implementationen ovan använder `system_clock` för att få nuvarande tid, konverterar det till `time_t`, sedan till en `tm` pekare som slutligen formateras till en sträng. Precis som många andra språk, hanterar C++ datum och tid som objekt före konvertering till sträng.

## See Also (Se Även)
- C++ `<chrono>` biblioteket: https://en.cppreference.com/w/cpp/header/chrono
- C++ `<iomanip>` beskrivning: https://en.cppreference.com/w/cpp/header/iomanip
- Boost DateTime bibliotek: https://www.boost.org/doc/libs/release/libs/date_time/
- JSON format specifikation: https://www.json.org/json-en.html
