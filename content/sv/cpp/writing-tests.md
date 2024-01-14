---
title:                "C++: Skriva tester"
programming_language: "C++"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/writing-tests.md"
---

{{< edit_this_page >}}

##Varför skriva tester i C++

Det finns många anledningar till varför det är viktigt att skriva tester när man programmerar i C++. Det är ett sätt att säkerställa att koden fungerar enligt förväntningarna och att den är robust och hållbar. Tester kan också hjälpa till att hitta och lösa buggar på ett tidigt stadium, vilket sparar både tid och pengar i längden.

##Hur man skriver tester i C++

För att skriva tester i C++ behöver du en testningsramverk, som till exempel "Catch2". Först måste du skapa ett separat testfil där du importerar testningsramverket och de filer som du vill testa. Sedan kan du skapa olika testfall med hjälp av assertioner, som kontrollerar om ett visst uttryck är sant eller falskt.

Ett exempel på hur det kan se ut:

```C++
#include <catch2/catch2.hpp>
#include "minKod.hpp"

TEST_CASE("Test av funktionen addera") {
    REQUIRE(addera(1, 2) == 3);
    REQUIRE(addera(5, 10) == 15);
    REQUIRE(addera(0, 0) == 0);
}
```

I det här fallet testar vi funktionen "addera" som vi har skapat i vår kodfil "minKod.hpp". Vi använder oss av assertionen "REQUIRE" för att säkerställa att resultatet är det förväntade.

##Djupdykning i skrivandet av tester

För att skriva effektiva tester är det viktigt att tänka på några grundläggande principer. En av dessa är att testerna ska vara isolerade från varandra, vilket betyder att ett test inte ska påverka ett annat test. Detta förhindrar oönskade bieffekter och gör det enklare att hitta och lösa problem.

Det är också viktigt att skapa testfall för olika scenarion, både för de förväntade och oönskade resultaten. På så sätt kan du vara säker på att din kod fungerar som den ska i olika situationer.

Testdriven utveckling (TDD) är en metod där man skriver tester först och sedan utvecklar koden utifrån dessa tester. Detta hjälper till att skapa en tydlig struktur och förhindrar onödiga buggar i koden.

##Se även

- [An Introduction to Catch2](https://github.com/catchorg/Catch2/blob/devel/docs/tutorial.md)
- [Test-Driven Development with Catch2](https://blog.jetbrains.com/clion/2017/05/test-driven-development-with-catch/)
- [Google Test and Google Mock](https://github.com/google/googletest)

Testning är en viktig del av utvecklingsprocessen och det är värt att lägga tid och resurser på att skapa bra tester. Med rätt verktyg och metoder kan du försäkra dig om att din kod är stabil och hållbar. Lycka till med dina tester!