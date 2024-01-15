---
title:                "Å skrive tester"
html_title:           "C#: Å skrive tester"
simple_title:         "Å skrive tester"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å skrive tester er en viktig del av utviklingen i ethvert programmeringsspråk. Tester hjelper oss med å sikre at koden vår fungerer som den skal og reduserer risikoen for feil og bugs. Det kan også bidra til å forbedre kvaliteten på koden og gjøre den mer pålitelig.

## Hvordan du gjør det

Skrive tester i C# er en enkel prosess som kan gjøres ved hjelp av .NET's innebygde testingverktøy, NUnit eller xUnit. Her er et eksempel på hvordan man kan skrive en enkel test i C#:

```C#
using NUnit.Framework;

[TestFixture]
public class CalculatorTests
{
    [Test]
    public void AddTwoNumbers_ReturnsCorrectSum()
    {
        // Arrange
        int num1 = 5;
        int num2 = 10;
        int expectedSum = 15;

        // Act
        int actualResult = Calculator.Add(num1, num2);

        // Assert
        Assert.AreEqual(expectedSum, actualResult);
    }
}
```

I koden ovenfor har vi opprettet en testklasse med en testmetode som sjekker om kalkulatoren vår gir riktig sum når vi legger sammen to tall. Vi bruker "Arrange, "Act" og "Assert" konvensjonen for å organisere koden vår og gjøre den mer leselig.

Når vi kjører testen vår, vil vi få en utgang som sier at testen har passert. Hvis vi endrer det ene av tallene våre i koden, vil testen mislykkes og vi vil få en feilmelding som indikerer at verdien ikke er som forventet.

## Dykk dypere

Når vi skriver tester, er det viktig å tenke på hva slags scenarier koden vår må kunne håndtere. Vi bør skrive tester for både positive og negative scenarier, for å sikre at koden vår fungerer som den skal under ulike forhold.

Vi kan også bruke "Mocks" og "Stubs" for å simulere ulike situasjoner og objekter når vi skriver tester. Dette kan være nyttig for å isolere koden vår og teste den uavhengig av andre avhengigheter.

Det finnes også ulike verktøy og teknikker som kan hjelpe oss med å skrive bedre tester, som for eksempel "Test Driven Development" (TDD) og "Behavior Driven Development" (BDD). Det kan være lurt å utforske disse konseptene for å forbedre testene våre enda mer.

## Se også

- [Microsoft sin guide for testing i .NET](https://docs.microsoft.com/en-us/aspnet/core/test/integration-tests?view=aspnetcore-5.0)
- [NUnit dokumentasjon](https://docs.nunit.org/)
- [xUnit dokumentasjon](https://xunit.net/)