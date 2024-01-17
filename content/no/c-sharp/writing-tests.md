---
title:                "Skriver tester"
html_title:           "C#: Skriver tester"
simple_title:         "Skriver tester"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive tester i C# er en måte å sikre at koden vår fungerer som den skal. Det innebærer å lage små programmer som kjører og sjekker funksjonaliteten til deler av vår hovedkode. Programmører gjør dette for å forsikre seg om at endringer eller tillegg til koden ikke ødelegger det som allerede fungerer, og for å finne og fikse eventuelle bugs før de blir et større problem.

## Slik gjør du:
Vi kan skrive tester ved å bruke spesifikke biblioteker i C#, som [NUnit](https://nunit.org/) eller [xUnit](https://xunit.net/). Her er et eksempel på en enkel test som sjekker om en metode returnerer riktig verdi:

```C#
// Importer det nødvendige biblioteket:
using NUnit.Framework;

// Definer en klasse for testene:
public class TestExample
{
  // En enkel testmetode:
  [Test]
  public void TestMethod()
  {
    // Definer input og forventet output:
    var input = 5;
    var expected = 10;

    // Kjør selve metoden som skal testes:
    var actual = Example.Method(input);

    // Sjekk om den faktiske outputen er lik den forventede:
    Assert.AreEqual(expected, actual);
  }
}
```

Output fra testen vil gi oss enten en grønn eller rød firkant, avhengig av om testen består eller ikke.

## Dypdykk:
Å skrive tester er ikke en ny praksis, det har vært brukt i lang tid i softwareutvikling. Alternativer til de nevnte bibliotekene inkluderer [MS Test](https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-mstest) og [SpecFlow](https://specflow.org/), blant mange andre. Det finnes også verktøy som automatiserer testingen, som [Selenium](https://www.selenium.dev/), som kan kjøre tester på en webapplikasjon.

Implementasjonen av tester kan variere, men det viktigste er å ha god dekning av koden vår. Vi bør teste både positive og negative scenarier, samt kanten tilfeller for å sikre at koden er robust. Det er også viktig å regelmessig kjøre testene våre for å fange opp eventuelle endringer som kan føre til bugs.

## Se også:
- [Introduction to Test-Driven Development (TDD)](https://www.youtube.com/watch?v=KyWkVyPr1rM)
- [Writing Unit Tests in C# with NUnit and Moq](https://www.pluralsight.com/guides/writing-unit-tests-csharp-nunit-moq)