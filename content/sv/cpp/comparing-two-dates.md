---
title:                "C++: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum är en viktig och vanlig uppgift inom programmering. Det kan användas för att sortera eller filtrera data, beräkna tidsintervaller och många andra tillämpningar. Genom att behärska hur man jämför datum i C++ så kommer du kunna lösa många olika programmeringsproblem.

## Hur man gör
För att jämföra två datum i C++ så använder man sig av datatypen "std::chrono::time_point". Detta är en specialiserad datatyp som används för att representera ett specifikt tidspunkt. Här är ett exempel på hur man skapar två datum och jämför dem:

```C++
#include <iostream>
#include <chrono> 

int main() 
{ 
    // Skapar två datum, ett för idag och ett för imorgon
    std::chrono::system_clock::time_point idag = std::chrono::system_clock::now(); 
    std::chrono::system_clock::time_point imorgon = idag + std::chrono::hours(24); 

    // Jämför datum
    if (imorgon > idag) { 
        std::cout << "Imorgon kommer efter idag." << std::endl; 
    }

    return 0; 
} 
```

Output:

```
Imorgon kommer efter idag.
```

## Deep Dive
För att förstå hur man jämför datum i C++ så är det viktigt att ha en grundläggande förståelse för hur tid representeras och mäts i datorsystem. I C++ så representeras tid med hjälp av "tidpunkter", vilket är en mängd sekunder som har gått sedan ett specifikt referenstillfälle kallat "epoch". I de flesta system så är epoch satt till 1 januari 1970 00:00:00 UTC.

När man jämför två datum i C++ så jämför man egentligen mängderna av sekunder som har gått sedan epoch. Det är därför viktigt att se till att båda tidpunkter man jämför är i samma tidszon, annars kan resultatet bli felaktigt.

En annan viktig punkt att tänka på när man jämför datum är att använda rätt datatyp för de olika typerna av tidpunkter man vill jämföra. Till exempel så är datatypen "std::chrono::system_clock::time_point" bäst lämpad för att representera aktuell tid, medan "std::chrono::steady_clock::time_point" är bättre för mätningar av tidsintervaller.

## Se även
- [Time library](https://en.cppreference.com/w/cpp/chrono)
- [C++ Date and Time - Tutorialspoint](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)
- [C++ Timestamps, System Time and Timers - Codeguru](https://www.codeguru.com/cpp/t-z/time/date.php)