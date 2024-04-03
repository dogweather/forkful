---
date: 2024-01-26 00:49:32.643247-07:00
description: "Hur man g\xF6r: H\xE4r \xE4r en grundl\xE4ggande try-catch block f\xF6\
  r att hantera ett undantag."
lastmod: '2024-03-13T22:44:38.216463-06:00'
model: gpt-4-1106-preview
summary: "H\xE4r \xE4r en grundl\xE4ggande try-catch block f\xF6r att hantera ett\
  \ undantag."
title: Hantering av fel
weight: 16
---

## Hur man gör:
Här är en grundläggande try-catch block för att hantera ett undantag:

```cpp
#include <iostream>
#include <stdexcept>

int main() {
    try {
        throw std::runtime_error("Oj då! Något gick fel.");
    } catch (const std::exception& e) {
        std::cerr << "Fel: " << e.what() << std::endl;
    }
    return 0;
}
```

Exempel på utskrift:
```
Fel: Oj då! Något gick fel.
```

## Fördjupning
C++ har haft felhantering sedan sina tidiga dagar. Den mest grundläggande formen var att kontrollera returvärden. Om du har varit med ett tag minns du tiden före standarden: C med klasser och manuell felkontroll.

Sedan kom undantag med C++ som gav oss ett strukturerat sätt att hantera oväntade problem. Ett undantag kastas med `throw` och fångas med `try/catch`.

Två typer av fel dyker ofta upp: logiska fel, som felaktiga beräkningar, och körningsfel, som att komma åt en ogiltig minnesadress. Undantag är idealiska för körningsfel. För logiska fel är det ofta bättre att använda påståenden (assertions) eller felkoder.

Det pågår en löpande debatt om undantag kontra felkoder. Undantag kan vara långsammare och kan leda till komplexa kontrollflöden. Felkoder, medan snabbare, kan göra koden rörig och svårare att underhålla. Det är en avvägning, så det är viktigt att känna till ditt användningsfall.

C++17 introducerade `std::optional` och `std::variant`, som är alternativ till undantag. De är användbara för funktioner som kan eller inte kan returnera ett giltigt resultat.

Undantagssäkerhet kan vara en annan huvudvärk. Det handlar om garantier din kod ger trots undantag. Det finns tre nivåer: grundläggande, stark och ingen kast (nothrow). Ju fler garantier, desto mer komplex kan din kod bli.

Sluttankar—felhantering är lika mycket konst som vetenskap. Det påverkar hur din applikation överlever i det vilda. Överanvänd inte undantag. Sikta på läsbar, underhållbar kod.

## Se även
- [cppreference om undantagshantering](https://en.cppreference.com/w/cpp/language/exceptions)
- [Bjarne Stroustrups syn på felhantering](http://www.stroustrup.com/except.pdf)
- [C++ Core Guidelines om undantag](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Re-exceptions)
