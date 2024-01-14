---
title:                "C++: Generering av slumpmässiga tal"
programming_language: "C++"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är en användbar teknik inom programmering, speciellt inom dataanalyser och spelutveckling. Genom att kunna skapa en serie av slumpmässiga nummer kan man simulera olika scenarion och förbättra algoritmer.

## Så här gör man

För att generera slumpmässiga nummer i C++, behöver man först inkludera biblioteket för slumpmässiga nummer ```<random>```. Sedan kan man använda funktioner såsom ```rand()``` för att få ett heltal eller ```double``` för att få ett decimaltal. Nedan är ett exempel på hur man kan generera 10 slumpmässiga tal mellan 1 och 100:

```C++
#include <iostream>
#include <random>

int main()
{
    // Skapar ett random generator objekt
    std::random_device rd;
    std::mt19937 gen(rd());
    // Definierar ett intervall mellan 1 och 100
    std::uniform_int_distribution<> distr(1, 100);

    // Genererar 10 slumpmässiga tal
    for (int i = 0; i < 10; i++)
    {
        std::cout << distr(gen) << std::endl;
    }

    return 0;
}
```

Output:

```
58
32
91
4
79
42
99
11
72
10
```

## Djupdykning

Vad händer egentligen bakom kulisserna när man genererar slumpmässiga nummer? I C++ används ofta en algoritm som heter Mersenne Twister, vilken skapar en lång sekvens av pseudoslumpmässiga tal baserat på en startpunktsseed. Denna seed kan sättas manuellt genom att kalla på ```srand()``` eller automatiskt genom att använda ett objekt från ```std::random_device```, som ofta ger en seed baserad på tiden på datorn.

Det finns också olika distributioner som kan användas för att reglera hur slumpmässiga nummer ska fördelas, såsom en jämn fördelning, normalfördelning eller binomialfördelning. Genom att använda rätt distribution kan man få mer kontrollerade och realistiska resultat från sina slumpmässiga nummer.

## Se även

- [En introduktion till Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)
- [Dokumentation för ```<random>``` biblioteket i C++](https://en.cppreference.com/w/cpp/numeric/random)