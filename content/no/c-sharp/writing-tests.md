---
title:    "C#: Skriver tester"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av god programmeringspraksis. Det hjelper utviklere med å sikre at koden deres fungerer som den skal og oppdage eventuelle feil før de påvirker produksjonen. Å skrive tester bidrar også til å forbedre kvaliteten på koden og gjøre det lettere å vedlikeholde den i fremtiden.

## Hvordan

```C#
using NUnit.Framework;

[TestFixture]
public class CalculatorTests
{
    [Test]
    public void Add_WhenIntegers_ReturnsSum()
    {
        // Arrange
        int num1 = 5;
        int num2 = 3;
        int expected = 8;

        Calculator calculator = new Calculator();

        // Act
        int result = calculator.Add(num1, num2);

        // Assert
        Assert.AreEqual(expected, result);
    }
}

public class Calculator
{
    public int Add(int num1, int num2)
    {
        return num1 + num2;
    }
}
```

I dette eksempelet bruker vi NUnit som testrammeverk for å skrive en enkel test som sjekker om summen av to tall blir korrekt beregnet. Vi starter med å opprette en testklasse og dekorere den med [TestFixture] attributtet. Deretter oppretter vi en testmetode som også er dekorert med [Test] attributtet. Inne i testmetoden bruker vi Assert.AreEqual() for å sammenligne den forventede verdien med resultatet fra å kalle metoden vi tester. Til slutt kjører vi testen ved å opprette en instans av Calculator-klassen og kalle Add() metoden.

## Dypdykk

Å skrive gode tester handler om mer enn bare å sjekke om koden fungerer. Det handler også om å designe testene på en måte som gir god dekning av koden og oppdager eventuelle feil og mangler. Det er viktig å tenke på ulike scenarier og kanttilfeller når du skriver tester, og å ha et godt forståelse av koden du tester. Ved å følge beste praksis og kontinuerlig forbedre testene dine, vil du kunne oppnå en mer pålitelig og robust kodebase.

## Se Også
- [NUnit Testing Framework](https://nunit.org)
- [Introduction to Unit Testing in C#](https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-nunit)
- [Writing Reliable and Readable Tests in C#](https://blog.gurock.com/writing-reliable-readable-tests-csharp/)