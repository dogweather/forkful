---
title:    "C++: Generera slumpmässiga nummer"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är en vanlig uppgift inom programmering, och kan vara användbart i en mängd olika scenarion. Oavsett om du behöver skapa en slumpmässig spelplan, ett lösenord eller någon annan sorts variation så kan slumpmässiga nummer hjälpa dig med detta.

## Så här

För att generera slumpmässiga nummer i C++ används funktionen `rand()`. För att använda denna funktion behöver du inkludera `cstdlib` i din kod. Sedan kan du använda `rand()` för att skapa ett slumpmässigt nummer. Se nedan för ett kodexempel:

```C++
#include <iostream>
#include <cstdlib>
using namespace std;

int main() {
    // Genererar ett slumpmässigt heltal mellan 1 och 100
    int random = rand() % 100 + 1;

    // Skriver ut det slumpmässiga numret
    cout << "Det slumpmässiga numret är: " << random << endl;

    return 0;
}
```

Exempeloutput:

```
Det slumpmässiga numret är: 57
```

Det är viktigt att komma ihåg att `rand()` inte genererar helt slumpmässiga nummer utan använder en algoritm för att skapa en sekvens av siffror som kan se slumpmässiga ut. Därför behöver du även inkludera `time.h` och använda funktionen `srand()` för att säkerställa att varje gång programmet körs så genereras en ny sekvens av nummer. Detta kan göras genom att använda tiden som en seed för `srand()`.

## Djupdykning

Som nämnts tidigare så genererar `rand()` inte helt slumpmässiga nummer. Istället används en algoritm som genererar en viss sekvens av nummer baserat på vilken seed som används. Detta kan ha vissa konsekvenser om du inte väljer en lämplig seed. Om samma seed används varje gång programmet körs så kommer samma sekvens av nummer att genereras varje gång.

En annan sak att tänka på är att `rand()` genererar heltal, vilket kan vara ett problem om du behöver generera decimaltal. För att göra detta kan du använda `static_cast<double>()` för att konvertera det slumpmässiga heltalet till ett decimaltal.

För att läsa mer om `rand()` och använda den på mer avancerade sätt så rekommenderas det att utforska dokumentationen för C++ eller titta på olika guider och exempel online.

## Se även

- [C++ Dokumentation - rand()](https://www.cplusplus.com/reference/cstdlib/rand/)
- [C++ Dokumentation - srand()](https://www.cplusplus.com/reference/cstdlib/srand/)
- [YouTube Tutorial - Slumpmässiga Nummer i C++](https://www.youtube.com/watch?v=8o9mV04BjLg)