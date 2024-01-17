---
title:                "Hämta aktuellt datum"
html_title:           "C++: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att få den nuvarande datumet är en vanlig uppgift för programmerare. Det innebär helt enkelt att hämta det aktuella datumet och tiden från systemets klocka. Detta behövs för att kunna spåra och logga tid för olika händelser eller för att visa datum och tid i användargränssnittet.

## Hur man gör:

```C++
#include <iostream>
#include <ctime>
using namespace std;

int main() {
  // Hämta det nuvarande datumet och tiden
  time_t nu = time(0);

  // Konvertera till sträng
  char* dt = ctime(&nu);

  // Skriv ut resultatet
  cout << "Aktuellt datum och tid är: " << dt << endl;

  return 0;
}
```
**Output:**
```
Aktuellt datum och tid är: Thu Jan 28 11:38:54 2021
```

## Deep Dive:

Att hämta det nuvarande datumet kan göras på olika sätt beroende på programvarulösningen och operativsystemet. I äldre versioner av C++ användes funktionen `getdate()` för att hämta det nuvarande datumet, men i den nuvarande versionen har detta ersatts av `ctime()`. Det finns också bibliotek som `chrono` som ger mer exakta och detaljerade funktioner för att hantera datum och tid. Det är viktigt att välja rätt metod beroende på dina specifika behov och användningsområden.

## Se även:

För mer information och exempel på hur man hanterar datum och tid i C++, se följande resurser:

- [cppreference - time](https://en.cppreference.com/w/cpp/chrono)
- [C++ Standard Library - Chrono](https://www.cplusplus.com/reference/chrono/)
- [GeeksforGeeks - Datum och tid inklusive biblioteket Chrono i CPP](https://www.geeksforgeeks.org/datetime-including-calendar-in-cpp/)