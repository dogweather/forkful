---
date: 2024-01-26 01:17:45.154748-07:00
description: "Hur man g\xF6r: T\xE4nk dig att du har en funktion som g\xF6r lite f\xF6\
  r mycket, som denna klumpiga metod som initierar ett objekt och \xE4ven utf\xF6\
  r loggning."
lastmod: '2024-03-13T22:44:38.217400-06:00'
model: gpt-4-0125-preview
summary: "T\xE4nk dig att du har en funktion som g\xF6r lite f\xF6r mycket, som denna\
  \ klumpiga metod som initierar ett objekt och \xE4ven utf\xF6r loggning."
title: Refaktorisering
weight: 19
---

## Hur man gör:
Tänk dig att du har en funktion som gör lite för mycket, som denna klumpiga metod som initierar ett objekt och även utför loggning:

```C++
#include <iostream>

class Widget {
public:
    void init(bool verbose) {
        // Initialiseringslogik
        // ...

        // Utförlig loggning
        if (verbose) {
            std::cout << "Widget initierad!" << std::endl;
        }
    }
};

// Användning:
Widget w;
w.init(true);
```

Utdata:
```
Widget initierad!
```

Att refaktorisera detta till renare, mer fokuserade metoder kan se ut så här:

```C++
#include <iostream>

class Widget {
public:
    void init() {
        // Endast initialiseringslogik
        // ...
    }

    void loggaInitiering() const {
        std::cout << "Widget initierad!" << std::endl;
    }
};

// Användning:
Widget w;
w.init();
w.loggaInitiering();
```

Denna ändring har inte förändrat vad programmet gör men gör `Widget`-klassen mer modulär och dess användning tydligare.

## Fördjupning
Konceptet med refaktorisering som vi känner till det idag har sina rötter i programmeringsgemenskaperna kring Smalltalk på 1980-talet och populariserades starkt av Martin Fowlers bok "Refactoring: Improving the Design of Existing Code" från 1999. Idag är refaktorisering en kärnkomponent i modern programvaruutveckling, integrerad i olika utvecklingsmetodologier som Agile och TDD (Test-Driven Development).

När vi talar om alternativ till refaktorisering hamnar vi i territoriet för omskrivning eller omformning. Refaktorisering är strategisk och inkrementell, medan en omskrivning kan skrota befintlig kod till förmån för en ny lösning. Omformningen kan däremot medföra mer betydande ändringar inklusive ändrad funktionalitet, vilket inte är ett mål för ren refaktorisering.

Implementeringsdetaljer om refaktorisering kan bli ganska detaljerade. Det finns många "kodlukter" som kan föranleda en refaktorisering, såsom långa metoder, stora klasser eller dupliserad kod. Automatiserade verktyg finns som kan hjälpa till med refaktorisering, såsom "Clang-Tidy" för C++, som kan upptäcka problem och till och med tillämpa vissa fixar.

Dessutom kräver refaktorisering en solid svit av tester för att säkerställa att funktionaliteten förblir oförändrad. Utan tester flyger du i princip blind och riskerar regressioner.

## Se också
För en djupare förståelse av refaktorisering och för att se fler exempel, kanske du vill kolla in:

- Martin Fowlers klassiska text "Refactoring: Improving the Design of Existing Code" för grundläggande idéer och strategier.
- `Clang-Tidy`-dokumentationen på https://clang.llvm.org/extra/clang-tidy/ för automatiserat stöd för refaktorisering i C++.
- "Working Effectively with Legacy Code" av Michael Feathers, som ger tekniker för säker refaktorisering i sammanhang av mindre perfekta befintliga kodbasar.
