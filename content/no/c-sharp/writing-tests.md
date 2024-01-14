---
title:                "C#: Skriving av tester"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av enhver programmeringsprosess. Det hjelper deg med å sikre at koden din fungerer som den skal, og det gjør det enklere å finne og fikse feil når de oppstår. Å skrive tester kan også være en god måte å dokumentere koden din på og gjøre det enklere for andre utviklere å forstå hva du har gjort.

## Hvordan

For å skrive tester i C# kan du bruke et rammeverk som heter NUnit. Dette er et populært rammeverk som gjør det enkelt å lage og kjøre tester. La oss se på et enkelt eksempel av hvordan du kan skrive en testklasse:

```C#
[TestClass]
public class CalculatorTests
{
    [TestMethod]
    public void Calculator_ShouldAddNumbersCorrectly()
    {
        // Arrange
        Calculator calculator = new Calculator();

        // Act
        int result = calculator.Add(3, 5);

        // Assert
        Assert.AreEqual(8, result);
    }
}
```

I dette eksempelet, oppretter vi en testklasse og en testmetode som sjekker om vår kalkulator-klasse legger sammen tallene riktig. Vi bruker NUnit sin `Assert.AreEqual`-metode for å sammenligne forventet resultat med det faktiske resultatet.

## Deep Dive

Det er mange ting du kan teste i en applikasjon, som for eksempel metoder, klasser, og grensesnitt. Men det er viktig å huske at tester skal være enkle, spesifikke og uavhengige av hverandre. Det er også viktig å teste både positive og negative scenarioer for å sikre at koden din håndterer alle mulige tilfeller.

NUnit har også mange nyttige funksjoner for å gjøre testingen din mer effektiv, som for eksempel data-driven testing og parallell testing. Det er også mulig å integrere NUnit med andre verktøy som for eksempel Visual Studio eller TeamCity for å automatisere testingen din.

## Se Også

- [NUnit offisiell nettside](https://nunit.org/)
- [En guide til test-drevet utvikling i C#](https://blog.testproject.io/2018/07/05/tdd-c-sharp/)
- [Hvordan skrive gode tester i C#](https://medium.com/@SergeyTeplyakov/writing-good-unit-tests-in-c-eaa10b0b4cde)