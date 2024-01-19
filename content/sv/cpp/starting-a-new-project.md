---
title:                "Att starta ett nytt projekt"
html_title:           "Arduino: Att starta ett nytt projekt"
simple_title:         "Att starta ett nytt projekt"
programming_language: "C++"
category:             "C++"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/starting-a-new-project.md"
---

{{< edit_this_page >}}

---
# ATTYA STARTA ETT NYTT C++-PROJEKT

## Vad och Varför?

Att starta ett nytt projekt är grunden för varje ny kodupplevelse. Det är hur programmerare skapar ny funktionalitet, löser problem och uttrycker kreativa idéer i kod.

## Hur till:

För att starta ett nytt C++ projekt, börja med att ställa in din arbetsmiljö och skapa en ny källkodsfil med ".cpp"-ändelsen.

```C++
// main.cpp
#include <iostream>

int main() {
    std::cout << "Hej, Världen!";
    return 0;
}
```

Kör din källkod för att se den utgångna utdata.

```C++
$ g++ main.cpp
$ ./a.out
Hej, Världen!
```

## Fördjupning

Historiskt sett har C++ programmerare skapat och arbetat med nya projekt för att bygga applikationer, utveckla spel, programvara för inbyggda system och mer.

Alternativt till C++, finns det andra programmeringsspråk som Python, Java, JavaScript, och Ruby. Valet av språk beror på projektets behov.

Implementation av ett nytt projekt i C++ innebär indelning av problemet i mindre delar (funktioner eller objekt), kodning av dessa delar separat och sedan sammansättning dem för att lösa det större problemet. Sådan modulär kod är lättare att underhålla och felsöka.

## Se Även

För mer information och kodexempel på C++:

- [Cplusplus.com](http://www.cplusplus.com)
- [Learn C++](https://www.learn-cpp.org/)
- [C++ Reference](https://en.cppreference.com/w/)

---