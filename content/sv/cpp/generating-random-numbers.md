---
title:                "C++: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är ett viktigt koncept inom programmering. Det låter dig skapa variation och slumpmässighet i ditt program, vilket kan vara användbart för spel, simuleringar och många andra applikationer. Att förstå hur man genererar slumpmässiga nummer är en viktig färdighet för varje programmerare.

## Hur man gör det

Generering av slumpmässiga nummer i C++ är enkelt med hjälp av standardbiblioteket `<cstdlib>`. För att skapa ett slumpmässigt heltal i ett visst intervall använder vi funktionen `rand()`, som genererar ett tal mellan 0 och `RAND_MAX` (en konstant som definieras i `<cstdlib>`). För att få ett heltal i ett specifikt intervall kan vi använda modulusoperatorn (%) på `rand()` tillsammans med intervallets storlek och sedan lägga till det lägsta värdet i intervallet.

```C++
#include <cstdlib>
#include <iostream>

int main() {
  // Slumpmässigt heltal mellan 1 och 10
  int randomNum = (rand() % 10) + 1;
  
  std::cout << "Ett slumpmässigt heltal: " << randomNum << std::endl;
  
  return 0;
}
```

För att skapa slumpmässiga flyttal kan vi använda funktionen `rand()` tillsammans med `RAND_MAX` och dividera resultatet med ett lämpligt tal för att få en lämplig decimalprecision.

```C++
// Slumpmässigt flyttal mellan 1.0 och 2.0
double randomFloat = (rand() / (double)RAND_MAX) + 1;
```

Vi kan också använda `srand()` för att seeda vår generator med ett startvärde, vilket ger oss möjlighet att få olika resultat varje gång programmet körs.

## Djupdykning

I C++ används en algoritm vid namn Linear Congruential Generator (LCG) för att generera slumpmässiga nummer med hjälp av `rand()`. Algoritmen utgör en matematisk formel som genererar en sekvens av nummer baserat på ett startvärde. Detta startvärde, eller seed, påverkar hur många och vilka slumpmässiga nummer som genereras. Om vi inte använder `srand()` kommer programmet att använda samma seed varje gång det körs, vilket resulterar i samma sekvens av slumpmässiga nummer varje gång.

Det finns också andra sätt att generera slumpmässiga nummer i C++, inklusive användning av tredjepartsbibliotek eller implementering av andra algoritmer. I vissa fall kan dessa alternativ ge bättre prestanda eller ett bredare utbud av slumpmässighet.

## Se även

- [C++ `<cstdlib>` referens (Engelska)](https://www.cplusplus.com/reference/cstdlib/)
- [Generering av slumpmässiga tal i C++ (Engelska)](https://www.geeksforgeeks.org/generating-random-number-range-c/)
- [En djupare titt på slumpmässiga nummer i C++ (Engelska)](https://medium.com/@denismakesgames/how-are-random-numbers-generated-in-c-11b8c0b1d722)

Tack för att du läst! Förhoppningsvis har denna artikel hjälpt dig att förstå vikten av slumpmässiga nummer och hur man enkelt kan implementera dem i dina C++ program. Fortsätt experimentera och utforska för att lära dig mer om generering av slumpmässiga nummer och andra viktiga koncept inom programmering. Lycka till!