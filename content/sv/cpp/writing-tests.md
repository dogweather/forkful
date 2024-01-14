---
title:    "C++: Skriva tester"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktigt del av utvecklingsprocessen i C++. Det hjälper dig att säkerställa att din kod fungerar korrekt och minskar risken för buggar och felaktigt beteende.

## Så här gör man

För att skriva tester i C++, behöver du använda ett ramverk som till exempel Google Test eller Catch2. Låt oss se ett enkelt exempel på hur du kan använda Catch2 för att skriva en testfunktion:

```C++
TEST_CASE("Addition") {
  int result = add(2, 3);
  REQUIRE(result == 5);
}
```

I detta exempel testas en funktion "add" som tar två heltal som parametrar och returnerar summan av dem. Med hjälp av "TEST_CASE" definierar vi en testfunktion med ett namn och sedan testar vi funktionen med hjälp av "REQUIRE" som kontrollerar att resultatet är det förväntade.

När vi kör detta test med hjälp av Catch2 kommer vi att se följande output:

```text
===============================================================================
All tests passed (1 assertion in 1 test case)
```

Detta visar att vårt test har gått igenom och allt fungerar som vi förväntar oss.

## Djupdykning

När du skriver tester är det viktigt att tänka på olika scenarier och gränsvärden för att täcka så mycket av din kod som möjligt. Det är också viktigt att kontrollera att felhantering fungerar korrekt och att dina tester är tillförlitliga.

Ett annat tips för att skriva bra tester är att använda sig av "AAA"-principen, vilket står för "Arrange, Act, Assert". Det innebär att du ska förbereda testet genom att sätta upp ett scenario, utföra testet och sedan kontrollera att resultatet är det förväntade.

Det finns många olika tekniker och strategier för att skriva tester och det bästa sättet är att utforska och experimentera för att hitta det som fungerar bäst för dig och ditt projekt.

## Se även

- [Google Test](https://github.com/google/googletest)
- [Catch2](https://github.com/catchorg/Catch2)
- [TDD i C++](https://www.bitdegree.org/learn/tdd-cpp)
- [Test-driven development för nybörjare](https://www.freecodecamp.org/news/test-driven-development-what-it-is-and-what-it-is-not-41fa6bca02a2/)