---
aliases:
- /nl/cpp/handling-errors/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:58.397821-07:00
description: "Fouten verwerken betekent plannen maken voor wanneer dingen verkeerd\
  \ gaan. Het is essentieel omdat het helpt crashes te voorkomen en je software robuust\u2026"
lastmod: 2024-02-18 23:09:02.191156
model: gpt-4-0125-preview
summary: "Fouten verwerken betekent plannen maken voor wanneer dingen verkeerd gaan.\
  \ Het is essentieel omdat het helpt crashes te voorkomen en je software robuust\u2026"
title: Fouten afhandelen
---

{{< edit_this_page >}}

## Wat & Waarom?
Fouten verwerken betekent plannen maken voor wanneer dingen verkeerd gaan. Het is essentieel omdat het helpt crashes te voorkomen en je software robuust en gebruiksvriendelijk maakt.

## Hoe te:
Hier is een basis try-catch blok om een uitzondering te verwerken:

```cpp
#include <iostream>
#include <stdexcept>

int main() {
    try {
        throw std::runtime_error("Oeps! Er ging iets fout.");
    } catch (const std::exception& e) {
        std::cerr << "Fout: " << e.what() << std::endl;
    }
    return 0;
}
```

Voorbeelduitvoer:
```
Fout: Oeps! Er ging iets fout.
```

## Diepere Duik
C++ heeft al vanaf zijn vroege dagen foutafhandeling. De meest basale vorm was het controleren van terugkeerwaardes. Als je al een tijdje meedraait, herinner je je de dagen voor de standaard: C met klassen en handmatige foutcontrole.

Toen kwamen uitzonderingen met C++ om ons een gestructureerde manier te geven om met onverwachte problemen om te gaan. Een uitzondering wordt gegooid met `throw` en gevangen met `try/catch`.

Twee soorten fouten komen vaak voor: logische fouten, zoals een verkeerde berekening, en runtimefouten, zoals toegang proberen te krijgen tot een ongeldig geheugenadres. Uitzonderingen zijn ideaal voor runtimefouten. Voor logische fouten is het vaak beter om beweringen of foutcodes te gebruiken.

Er is een aanhoudend debat over uitzonderingen versus foutcodes. Uitzonderingen kunnen langzamer zijn en kunnen leiden tot complexe controlestromen. Foutcodes, hoewel sneller, kunnen code rommelig en moeilijker te onderhouden maken. Het is een afweging, dus het kennen van je use case is de sleutel.

C++17 introduceerde `std::optional` en `std::variant`, die alternatieven zijn voor uitzonderingen. Ze zijn nuttig voor functies die mogelijk geen geldig resultaat teruggeven.

Uitzonderingsveiligheid kan een andere hoofdpijn zijn. Het gaat over garanties die je code biedt ondanks uitzonderingen. Er zijn drie niveaus: basis, sterk, en nothrow. Hoe meer garanties, hoe complexer je code kan zijn.

Laatste gedachten - foutenafhandeling is zowel kunst als wetenschap. Het vormt hoe je applicatie overleeft in de wildernis. Gebruik uitzonderingen niet te veel. Streef naar leesbare, onderhoudbare code.

## Zie Ook
- [cppreference over foutafhandeling](https://en.cppreference.com/w/cpp/language/exceptions)
- [Bjarne Stroustrup's visie op foutafhandeling](http://www.stroustrup.com/except.pdf)
- [C++ Core Richtlijnen over uitzonderingen](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Re-exceptions)
