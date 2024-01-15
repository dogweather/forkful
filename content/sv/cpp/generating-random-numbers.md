---
title:                "Skapa slumpmässiga nummer"
html_title:           "C++: Skapa slumpmässiga nummer"
simple_title:         "Skapa slumpmässiga nummer"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför
Randomisering är en viktig del av programmering eftersom den gör det möjligt att skapa variation och slumpmässighet i våra program. Detta kan vara användbart för att testa och optimera kod eller för att skapa spel eller lotteridragningar.

## Hur man skapar slumpmässiga nummer i C++
För att skapa slumpmässiga nummer i C++, måste vi använda oss av det inbyggda biblioteket `<cstdlib>` och dess funktion `rand()`. Börja med att inkludera biblioteket och sedan använda funktionen för att generera ett slumpmässigt tal mellan 0 och `RAND_MAX`.

```C++
#include <cstdlib>

// Generera ett slumpmässigt tal mellan 0 och RAND_MAX
int randomNumber = rand();

// För att få ett värde inom ett visst intervall, kan vi använda modulus-operatorn
// I detta exempel genererar vi ett tal mellan 1 och 10
int randomNumberInRange = rand() % 10 + 1; 
```

För att säkerställa att vi får olika nummer varje gång programmet körs, måste vi initialisera slumpgeneratorn med ett seed-värde. Detta kan göras genom att använda funktionen `srand()` tillsammans med ett unikt värde, som t.ex. tiden i sekunder.

```C++
// Initialisera slumpgeneratorn med seed-värde baserat på tiden i sekunder
srand(time(NULL));

// Nu kommer varje gång programmet körs att slumpgeneratorn initieras med ett nytt värde
```

## Djupdykning
Det finns en hel del saker att tänka på när det kommer till att generera slumpmässiga nummer i C++. Här är några vidarefördjupningar för de som är intresserade:

- Du kan använda funktionen `srand()` för att återställa slumpgeneratorn och skapa en sekvens av samma slumpmässiga tal varje gång.
- För att undvika att använda modulus-operatorn kan du använda `RAND_MAX` som en skalningsfaktor för att få ett tal inom ett visst intervall.
- Funktionen `rand()` säljer inte tillförlitliga slumpmässiga nummer eftersom det är beroende av seed-värdet. Det finns andra, bättre sätt att generera slumpmässiga nummer i C++ som t.ex. `uniform_int_distribution` från `<random>`-biblioteket.

## Se även
- Dokumentationen för `<cstdlib>` [https://www.cplusplus.com/reference/cstdlib/](https://www.cplusplus.com/reference/cstdlib/)
- En tutorial om slumpmässiga tal i C++ [https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- The C++ Random Number Library [https://isocpp.org/files/papers/n3557.pdf](https://isocpp.org/files/papers/n3557.pdf)