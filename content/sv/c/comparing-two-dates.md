---
title:                "Jämföring av två datum"
html_title:           "C: Jämföring av två datum"
simple_title:         "Jämföring av två datum"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum är en vanlig uppgift inom programmering. Det kan vara användbart för att kontrollera giltigheten av användarinmatningar, sortera data eller för att skapa dynamiska funktioner baserade på en specifik tidsperiod.

## Så här gör du
För att jämföra två datum i C kan man använda funktionen `difftime()`. Den tar in två tidvärden och returnerar differensen i sekunder. Här är ett exempel på hur man kan använda `difftime()` för att jämföra två datum och skriva ut resultatet.

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Skapa två strukturer för att hålla tidvärden
    struct tm tidA;
    struct tm tidB;

    // Använd funktionen localtime() för att fylla i tidstrukturer för två datum
    strptime("2021-06-01", "%Y-%m-%d", &tidA);
    strptime("2021-06-15", "%Y-%m-%d", &tidB);

    // Använd difftime() för att jämföra de två datum och spara resultatet i en variabel
    double differens = difftime(mktime(&tidB), mktime(&tidA));

    // Skriv ut resultatet i sekunder
    printf("Skillnaden mellan de två datum är %.0f sekunder\n", differens);

    return 0;
}
```

**Output:**

```
Skillnaden mellan de två datum är 1209600 sekunder
```

## Djupdykning
För att förstå mer om hur `difftime()` fungerar kan det vara bra att förstå hur tiden representeras och lagras i C-programmering. I C finns det tre huvudsakliga tidsrepresentationer:

- `time_t` - En heltalsvariabel som representerar antalet sekunder som har gått sedan 1 januari 1970.
- `struct tm` - En struktur som innehåller fält för datum, tid och andra tidsrelaterade värden.
- `struct timeval` - En struktur som innehåller fält för antalet sekunder och mikrosekunder.

Funktionen `mktime()` används för att konvertera en `struct tm` till en `time_t`. Detta är användbart när man behöver jämföra två datum eftersom `time_t` är enklare att jämföra än en struktur med flera värden. Därefter kan `difftime()` användas för att räkna ut skillnaden mellan två `time_t`-värden i sekunder.

## Se även
- [C Time and Date Library](http://www.nongnu.org/tct/) - En omfattande C-bibliotek för hantering av datum och tid.
- [C Reference - difftime()](https://www.tutorialspoint.com/c_standard_library/c_function_difftime.htm) - En komplett beskrivning av difftime-funktionen.