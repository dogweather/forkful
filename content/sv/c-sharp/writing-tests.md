---
title:    "C#: Skriva tester"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Varför

När man skriver kod är det viktigt att också inkludera tester i processen. Genom att skriva tester kan man försäkra sig om att koden fungerar som den ska och att eventuella ändringar inte orsakar problem. Det är med andra ord ett sätt att säkerställa kvaliteten på sin kod. I denna bloggpost kommer vi att ta en titt på hur man skriver tester i C#.

## Så här gör du

För att skriva tester i C# behöver man en testramverk. Ett populärt val är NUnit, men det finns också andra alternativ som xUnit och MSTest. Här är ett enkelt exempel på hur man kan skriva en testklass med NUnit:

```C#
[TestFixture]
public class CalculatorTests
{
    [Test]
    public void Add_TwoNumbers_ReturnsSum()
    {
        // Arrange
        var calculator = new Calculator();

        // Act
        var result = calculator.Add(2, 3);

        // Assert
        Assert.AreEqual(5, result);
    }
}
```

I exemplet ovan har vi en testklass som heter `CalculatorTests` och en testmetod som heter `Add_TwoNumbers_ReturnsSum`. I `Arrange`-delen förbereder vi vår kod genom att skapa en instans av `Calculator`-klassen. Sedan i `Act`-delen utför vi den faktiska beräkningen genom att anropa `Add`-metoden med två tal som argument. Slutligen i `Assert`-delen kontrollerar vi att resultatet är korrekt med hjälp av `Assert.AreEqual`-metoden.

För att köra testet kan vi använda testrunnern som finns inbyggd i verktyget Visual Studio. Om testet lyckas får vi ett grönt lysande meddelande, annars får vi ett rött meddelande med info om var testet misslyckades och vad som gick fel.

## Djupdykning

Att skriva tester handlar inte bara om att få gröna lysande meddelanden. Det handlar också om att bygga upp ett robust och pålitligt testsvit som täcker alla delar av koden. Det finns flera principer och tekniker som kan hjälpa till med detta.

En viktig princip är "Enhetstestning". Det innebär att man testar enskilda enheter av koden, till exempel en metod eller klass, istället för hela applikationen. Genom att testa enheterna separat kan man identifiera och åtgärda eventuella problem tidigare i processen.

En annan viktig teknik är "Mocking". Det innebär att man kan simulera och styra behavior hos beroenden istället för att använda de faktiska beroendena. Detta är särskilt användbart när man testar klasser som är beroende av andra klasser eller objekt.

## Se även

- [NUnit](https://nunit.org/)
- [xUnit](https://xunit.net/)
- [MSTest](https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-mstest)
- [Moq](https://github.com/Moq/moq4)
- [Enhetstestning i C# med NUnit](https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-nunit)
- [Bästa praxis för enhetstestning i C#](https://www.infoq.com/articles/unit-testing-basics-csharp/)