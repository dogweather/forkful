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

## Varför

Att skriva enhetstester är en viktig del av den moderna programmeringsprocessen. Det hjälper till att upptäcka buggar tidigt och garanterar att koden fungerar som den ska.

## Hur man gör

Det finns flera sätt att skriva enhetstester i C#, men det mest populära är att använda ramverket NUnit. Här är ett exempel på hur man kan skapa ett enkelt enhetstest:

```
using NUnit.Framework; //importera NUnit-framework

[TestFixture] //testklass som behövs för att NUnit ska identifiera testmetoder
public class CalculatorTests
{
  [TestCase(2, 3, 5)] //testmetod som tar tre parametrar: x, y och förväntat resultat
  [TestCase(5, 5, 10)]
  [TestCase(-3, 3, 0)]
  public void Add_TwoNumbers_ReturnsSum(int x, int y, int expected)
  {
    //förbereda
    Calculator calculator = new Calculator();

    //utföra
    int result = calculator.Add(x, y);

    //utvärdera
    Assert.AreEqual(expected, result); //jämför förväntat resultat med faktiskt resultat
  }
}
```

Dessa testmetoder kommer att köra samma kod flera gånger, men med olika värden för x, y och förväntat resultat. Om något av testen misslyckas, så kommer NUnit att ge dig information om vilken testmetod som gick fel och varför.

## Deep Dive

För att skriva effektiva enhetstester behöver man följa några enkla regler:

1. Skriv testet först: Skriv testet innan du skriver koden. Detta hjälper dig att fokusera på vad koden ska göra och försäkrar att du verkligen testar den.

2. Testa alla möjliga scenarion: Se till att dina tester täcker alla delar av koden, inklusive alla möjliga indata och utfall.

3. Använd rätt verktyg: Det finns många olika ramverk och verktyg för att skriva enhetstester i C#. NUnit är det mest populära, men det finns också andra som xUnit och MSTest.

4. Håll testet läsbart: Det är viktigt att dina test är enkla att förstå och läsa för att kunna diagnostisera fel lättare.

5. Testa även gränserna: Testa både förväntade och oväntade indata för att se hur koden beter sig. Detta hjälper till att hitta potentiella buggar eller problemområden.

Att skriva enhetstester är ett ständigt pågående arbete och det är viktigt att fortsätta att testa koden även när den har gått igenom ändringar. Det är också viktigt att hålla testerna uppdaterade och relevanta efter varje ändring i koden.

## Se även

Här är några användbara länkar för att lära dig mer om att skriva enhetstester i C#:

- Officiell NUnit dokumentation: https://nunit.org/
- xUnit.net dokumentation: https://xunit.net/
- C# MSTest guide: https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-mstest