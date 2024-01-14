---
title:                "C#: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Varför
Varför ska man bry sig om att skriva tester när man kodar? För det första är tester ett sätt att säkerställa att koden fungerar som den ska och förhindrar eventuella buggar från att dyka upp i produktion. Dessutom kan testning hjälpa till att bryta ner stora problem i mindre, mer hanterbara delar och öka effektiviteten i utvecklingsprocessen.

## Hur man gör
Att skriva tester kan verka som en tidskrävande uppgift, men det är en investering som kommer att löna sig i längden. Det finns olika typer av tester som man kan skriva, men i denna artikel kommer vi att fokusera på enhetstester, vilket är tester som fokuserar på enskilda delar av koden.

### Enhetstester
Enhetstester är tester som testar enskilda metoder eller funktioner i koden. För att skriva enhetstester i C# behöver vi en testram, som till exempel NUnit eller xUnit. För att installera NUnit, öppna Package Manager Console i Visual Studio och skriv in följande kommando:

```
Install-Package NUnit
```

Sedan behöver vi skapa ett testprojekt där vi kan skriva våra tester. För att göra det, högerklicka på din lösning och välj "Add" > "New Project". Välj sedan "Test" under Visual C# och välj "NUnit Test Project" som mall. Nu är du redo att skriva dina första tester.

För att skapa en testmetod, skriv följande kod:

```C#
[Test]
public void AddTwoNumbers_ShouldReturnCorrectSum()
{
    // Arrange
    var num1 = 5;
    var num2 = 10;

    // Act
    var result = Calculator.AddTwoNumbers(num1, num2);

    // Assert
    Assert.AreEqual(15, result);
}
```

I detta exempel har vi en testmetod som testar metoden "AddTwoNumbers" i vår "Calculator" klass. Vi förbereder våra testvariabler (Arrange), kör testet (Act) och kontrollerar sedan om resultatet är korrekt (Assert).

För att köra testet, högerklicka på metoden och välj "Run Tests".

### Mocking
I vissa fall kan det vara nödvändigt att skapa mock-objekt för att testa vår kod. Till exempel, om vår kod använder en extern resurs som en databas eller ett API, vill vi inte att våra tester ska påverka den faktiska resursen. För att lösa detta problem kan vi använda oss av ett mockningsramverk som till exempel Moq eller NSubstitute.

För ett exempel på hur man kan använda mockning, se denna [tutorial](https://docs.microsoft.com/en-us/ef/ef6/fundamentals/testing/mocking) från Microsoft.

## Djupdykning
Att skriva tester är en konst som tar tid att bemästra. Det finns många olika tekniker och strategier som man kan använda för att skriva effektiva tester. En av de viktigaste strategierna är att hålla testen enkla och lätta att förstå. Om du behöver mycket kod för att skriva ett test kan det vara ett tecken på att din kod kanske behöver brytas ner i mindre delar.

En annan viktig faktor är att skriva testbara kod. Detta betyder att din kod bör vara separat från externa resurser och beroenden, så att du enkelt kan mocka dem för dina tester.

## Se även
- [NUnit](https://nunit.org/)
- [xUnit](https://xunit.net/)
- [Moq](https://github.com/moq/moq4)
- [NSubstitute](https://nsubstitute.github.io/)