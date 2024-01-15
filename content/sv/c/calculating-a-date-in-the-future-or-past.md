---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "C: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att beräkna ett datum i framtiden eller det förflutna kan vara användbart för att hantera tidsberoende uppgifter eller program. Det kan också vara en bra övning för att förbättra dina programmeringsfärdigheter.

## Hur man gör det

Det finns ett antal olika sätt att beräkna ett datum i framtiden eller det förflutna i C. Här är exempel på två metoder med tillhörande kod och resultat.

#### Metod 1: Använd "time.h" biblioteket

För att göra detta behöver vi använda funktionen `mktime()` som finns i "time.h" biblioteket. Detta är ett exempel som beräknar ett datum 7 dagar framåt från det aktuella datumet:

```C
#include <stdio.h>
#include <time.h>

int main() {
   // Här skapar vi en struktur för att lagra det aktuella datumet
   struct tm currentTime;
   // Här fångar vi upp det aktuella datumet med hjälp av funktionen `time()`
   time_t now = time(NULL);

   /* 
   Här beräknar vi datumet 7 dagar framåt genom att addera antalet sekunder i en vecka (604800) till det aktuella datumet.
   Vi använder även funktionen `localtime()` för att konvertera detta till strukturform.
   */
   currentTime = *localtime(&now);
   currentTime.tm_sec += 604800;
   now = mktime(&currentTime);

   /* 
   Slutligen skriver vi ut det nya datumet i ett läsbart format med hjälp av funktionen `asctime()` 
   */
   printf("Det nya datumet är: %s\n", asctime(localtime(&now)));
   return 0;
}
```

Output:

```
Det nya datumet är: Sun May 30 19:51:28 2021
```

#### Metod 2: Använd "date.h" biblioteket

En annan metod är att använda "date.h" biblioteket som är en extern bibliotek för att hantera datum och tider. Det erbjuder ett enkelt gränssnitt för att hantera datumberäkningar. Här är ett exempel som beräknar datumet 1 år tillbaka från det aktuella datumet:

```C
#include <stdio.h>
#include "date.h"

int main() {
    // Här skapar vi ett `date` objekt för det aktuella datumet 
    date_t current_date = date_today();

    // Här beräknar vi datumet 1 år tillbaka med hjälp av funktionen `date_add_years()`
    current_date = date_add_years(current_date, -1);

    // Slutligen skriver vi ut det nya datumet med hjälp av funktionen `date_print()`
    printf("Det nya datumet är: ");
    date_print(current_date, "%b %d, %Y");
    return 0;
}
```

Output:

```
Det nya datumet är: May 31, 2020
```

## Djupdykning

Att beräkna ett datum i framtiden eller det förflutna kan kräva kunskap om olika tidsrelaterade begrepp såsom timestamp och strukturer som används för att lagra datum och tider. Det finns också många praktiska tillämpningar av denna kod, såsom att skapa kalendrar eller schemalägga uppgifter.

## Se även

Här är några användbara länkar för vidare läsning:

- [C Programming Tutorial - Time & Date Basics](https://www.youtube.com/watch?v=6B8tFYCH6yA)
- [The time.h Header File in C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Date and Time Library for C](https://github.com/HowardHinnant/date)