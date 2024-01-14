---
title:    "C++: Skriva tester"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Varför
Att skriva kod är en spännande process som kräver kreativitet och problem-lösningstänkande. Men ibland kan denna process bli frustrerande när kod som vi trodde skulle fungera inte gör det. Att skriva tester kan hjälpa till att undvika dessa problem och säkerställa att vår kod fungerar som det är tänkt. I denna bloggpost kommer vi att titta närmare på hur man skriver tester i C++.

## Hur man skriver tester i C++
Att skriva tester i C++ innebär att vi skriver en kod som testar vår befintliga kod. Det finns olika typer av tester som vi kan använda, men vi ska fokusera på enhetstester i denna bloggpost. En enhetstest är ett test som fokuserar på att testa en enskild enhet av vår kod, vanligtvis en funktion eller metod. 

För att skriva ett enhetstest behöver vi ett testramverk, som hjälper oss att strukturera och organisera våra tester. Ett populärt testramverk för C++ är Google Test. Detta ramverk ger oss verktyg och funktioner för att skriva effektiva tester.

Låt oss ta ett enkelt exempel på en funktion som adderar två tal och skriva ett enhetstest för den. Vi kommer att använda Google Test för vårt exempel. 

```C++
#include <gtest/gtest.h> // inkluderar Google Test biblioteket
#include "calculator.h" // inkluderar vår fil med adderingsfunktionen

TEST(AdditionTest, AddsTwoNumbersCorrectly) { // Definierar ett testfall
    // ARRANGE: Förbereder våra testdata och förväntat resultat
    int num1 = 5;
    int num2 = 10;
    int expectedResult = 15;

    // ACT: Utför vår adderingsfunktion med testdatan
    int result = add(num1, num2);

    // ASSERT: Kontrollerar om resultatet är samma som förväntat
    ASSERT_EQ(expectedResult, result);
}

int main(int argc, char* argv[]) {
    // Initialiserar vårt testprogram
    testing::InitGoogleTest(&argc, argv);

    // Kör våra tester
    return RUN_ALL_TESTS();
}
```

I vårt exempel använder vi makron från Google Test som hjälper oss att strukturera vårt testfall. Förberedelser görs i "ARRANGE" delen, funktionen som ska testas anropas i "ACT" delen, och slutligen kontrolleras resultatet i "ASSERT" delen. I vårt fall är resultatet rätt, och vårt test passerar.

Det är viktigt att notera att vårt testfall bara testar en specifik del av vår kod. Detta gör det möjligt för oss att isolera eventuella fel och felsöka dem enklare.

## Djupdykning
Att skriva tester kan verka som en tidsödande process, men det kan hjälpa till att upptäcka potentiella fel tidigt i utvecklingsprocessen. Det kan också ge oss mer självsäkerhet när vi gör ändringar i vår kod, eftersom vi vet att våra befintliga tester kommer att upptäcka eventuella fel som kan uppstå.

När vi skriver tester, är det viktigt att tänka på våra testfalls täckning. Detta innebär att se till att vi testar alla olika vägar och scenarier inom vår kod. Ju högre täckningsgrad vi har, desto säkrare kan vi vara på att vår kod fungerar korrekt.

Det är också viktigt att välja rätt nivå av tester. Enhetstester är bra för att testa små enheter av vår kod, men vi bör också använda andra typer av tester, såsom integrationstester och systemtester, för att testa hela vårt program.

## Se även
- [Getting Started with Google Test (engelska)](https://github.com/google/googletest/blob/master/googletest/docs/Primer.md)
- [C++ testing libraries (engelska)](https://github.com/onqtam/awesome