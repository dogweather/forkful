---
title:                "Generering av slumpmässiga nummer"
html_title:           "C++: Generering av slumpmässiga nummer"
simple_title:         "Generering av slumpmässiga nummer"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumpmässiga tal är en vanlig uppgift för programmerare. Det är en metod som används för att få fram ett slumpvist resultat, vilket kan vara användbart för att testa program, skapa spel eller för att skapa en variation i data.

## Hur man gör:
Det finns flera sätt att generera slumpmässiga tal i C++, men den vanligaste metoden är att använda funktionen `rand()` från standardbiblioteket `<cstdlib>`.

```C++
#include <iostream>
#include <cstdlib>

using namespace std;

int main()
{
    // Generera ett slumpmässigt tal mellan 1 och 100
    int random_num = rand() % 100 + 1;
    
    // Skriv ut det slumpmässiga talet
    cout << "Det slumpmässiga talet är: " << random_num << endl;
    
    return 0;
}
```

Output:
```
Det slumpmässiga talet är: 76
```

För att generera ett slumpmässigt tal mellan två specifika värden, kan man använda formeln `rand() % (max - min + 1) + min`.

```C++
#include <iostream>
#include <cstdlib>

using namespace std;

int main()
{
    // Generera ett slumpmässigt tal mellan 50 och 100
    int min = 50;
    int max = 100;
    
    int random_num = rand() % (max - min + 1) + min;
    
    // Skriv ut det slumpmässiga talet
    cout << "Det slumpmässiga talet är: " << random_num << endl;
    
    return 0;
}
```

Output:
```
Det slumpmässiga talet är: 83
```

## Deep Dive:
Att generera slumpmässiga tal har funnits med i programmering sedan tidigt 1900-tal och startade med enkel matematik för att få fram ett till synes slumpmässigt resultat. Idag finns det flera avancerade algoritmer och metoder för att generera slumpmässiga tal, men grunden är fortfarande densamma.

Alternativet till att använda `rand()`-funktionen är att använda bibliotek som till exempel Boost, som erbjuder fler avancerade möjligheter för att generera slumpmässiga tal.

För att förbättra slumpmässigheten hos de genererade talen är det viktigt att använda en "seed", som är ett startvärde för algoritmen. Detta startvärde behöver variera för varje nytt program som körs, annars kommer samma sekvens av slumpmässiga tal att genereras varje gång.

## Se även:
- [C++ rand() reference](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Boost random library](https://www.boost.org/doc/libs/1_76_0/doc/html/boost_random.html)