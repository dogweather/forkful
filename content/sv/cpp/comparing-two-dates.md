---
title:                "Jämföra två datum"
html_title:           "C++: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Jämföra två datum kan vara användbart för programmörer när man behöver identifiera skillnaden mellan två olika datum eller för att se om ett datum ligger före eller efter ett annat. Detta kan vara särskilt viktigt när man arbetar med tidsberoende algoritmer eller behöver sortera data baserat på datum.

## Hur?
För att jämföra två datum i C++ använder man sig oftast av funktionen ```std::chrono::time_point```, som representerar ett specifikt tidspunkt i tiden. Här är ett exempel på hur man kan jämföra två datum och skriva ut resultatet:

```{C++}
#include <iostream>
#include <chrono>

int main() {
  auto datum1 = std::chrono::system_clock::now(); //första datumet
  auto datum2 = std::chrono::system_clock::now(); //andra datumet
  
  if (datum1 > datum2){
    std::cout << "Datum 1 är senare än datum 2" << std::endl;
  } else if (datum1 < datum2) {
    std::cout << "Datum 1 är tidigare än datum 2" << std::endl;
  } else {
    std::cout << "Datum 1 är samma som datum 2" << std::endl;
  }
  
  return 0;
}
```

Output:
```
Datum 1 är samma som datum 2
```

## Djupdykning
Jämförelse av datum har blivit enklare med introduktionen av ```std::chrono``` biblioteket i C++. Före detta var man tvungen att använda sig av tidsfunktioner som till exempel ```time.h``` eller ```ctime``` för att hantera datum. Detta bibliotek ger exakta tidsenheter och är som standard inställd på koordinerad universell tid (UTC).

Som alternativ till ```std::chrono::time_point``` finns också ```std::chrono::duration``` som kan användas för att beräkna tidsdifferenser.

## Se även
- [C++ reference - <chrono>](https://en.cppreference.com/w/cpp/chrono)
- [C++ Foundation - Jämföra tider](https://isocpp.org/blog/2015/07/jaring-tider)
- [Time and date utilities in C++](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)