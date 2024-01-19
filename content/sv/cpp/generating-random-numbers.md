---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generera Slumpmässiga Nummer i C++

## Vad & Varför?

Generera slumpmässiga nummer innebär att skapa nummer på ett icke-deterministiskt sätt i kod. Detta är väsentligt i programmering för mönsterbrytning, simuleringar, spill, tester och mycket mer.

## Hur man:

Att generera slumpmässiga nummer i C++ är rättfram med `<random>` biblioteket. Här är ett snabbt exempel:

```C++
#include <random>
#include <iostream>

int main() {
    std::random_device rd;  // Seed for our random number generator
    std::mt19937 eng(rd()); // Mersenne Twister engine
    std::uniform_int_distribution<> distr(0, 100); // Define the range

    for(int n=0; n<10; n++)
        std::cout << distr(eng) << ' '; // Output random numbers
}
```

Det här programmet kommer att skriva ut tio slumpmässiga nummer mellan 0 och 100.

## Djupdykning

Generera slumpmässiga nummer i datorprogram är inte helt nytt och går tillbaka till tidiga datorer. Programmerare har alltid behövt sätt att skapa unikhet eller slumpmässighet i sina applikationer.

C++ har mer än ett sätt att generera slumpmässiga nummer. Ett alternativ till `<random>` biblioteket är `rand()` funktionen från `<cstdlib>`. `rand()` är dock äldre och har begränsningar, så `<random>` föredras oftast.

Det är viktigt att notera att `std::random_device` inte garanterar att varje körning kommer att producera olika sekvenser. För verkligt "slumpmässiga" sekvenser kan tiden eller något annat unikt värde, såsom systemstatistik, användas som frö.

## Se även

- C++ referens om `<random>` biblioteket: [här](http://www.cplusplus.com/reference/random/)
- Förståelse och användning av frön i slumpmässig nummergenerering: [artikel](https://www.eg.bucknell.edu/~xmeng/Course/CS6337/Note/master/node37.html)
- Jämförelse mellan `rand()` och `<random>`: [diskussion](https://stackoverflow.com/questions/39288595)