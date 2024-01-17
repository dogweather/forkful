---
title:                "Skriva tester"
html_title:           "C#: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva tester är en viktig del av utvecklingsprocessen för programvaror. Det innebär att skriva kod som testar den befintliga koden för att upptäcka eventuella fel eller buggar. Detta är viktigt eftersom det hjälper programmerare att identifiera och åtgärda problem innan koden implementeras i en produktionsmiljö.

## Hur man gör:
För att skriva tester i C# måste du först inkludera ett testramverk som NUnit, MS Test eller xUnit i ditt projekt. Sedan kan du skriva tester i form av metoder som kallas "testmetoder". Dessa metoder börjar vanligtvis med "test" och kan ha olika parametrar beroende på vad som ska testas. Nedan följer ett exempel på en testmetod som kontrollerar om en funktion returnerar det förväntade resultatet:

```C#
[Test]
public void TestAddition()
{
    // Arrange
    int num1 = 5;
    int num2 = 10;

    // Act
    int result = Calculator.Add(num1, num2);

    // Assert
    Assert.AreEqual(15, result);
}
```

## Djupdykning:
Att skriva tester har funnits i många år men har blivit allt viktigare i den moderna utvecklingsmiljön. Förutom att hjälpa till att upptäcka fel och buggar, kan tester också fungera som dokumentation för koden och underlätta refaktorering. Exempel på andra populära testramverk för C# inkluderar NUnit, MSTest och xUnit. Det finns också alternativ till testning av kod, som t.ex. användartester och kontinuerlig integration.

## Se även:
- [NUnit](https://nunit.org/)
- [MS Test](https://docs.microsoft.com/en-us/visualstudio/test/writing-unit-tests-for-c-cpp?view=vs-2019)
- [xUnit](https://xunit.net/)