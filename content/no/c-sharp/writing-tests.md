---
title:                "C#: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av utviklingsprosessen. Det hjelper deg å oppdage og fikse feil tidligere, noe som resulterer i mer stabil og pålitelig kode. Ved å skrive tester, kan du også dokumentere koden din og bidra til økt forståelse for andre utviklere.

Nå lurer du kanskje på hvordan du skal skrive tester i C#, så la oss dykke inn i det neste avsnittet.

## Hvordan skrive tester i C#

For å skrive tester i C#, trenger du et testrammeverk som NUnit eller xUnit og et verktøy som Visual Studio eller Visual Studio Code. La oss se på et eksempel på hvordan du kan skrive en enkel enhetstest ved hjelp av NUnit:

```
using System;
using NUnit.Framework;

namespace TestProject
{
  public class Calculator
  {
    public int Add(int a, int b)
    {
      return a + b;
    }
  }

  [TestFixture]
  public class CalculatorTests
  {
    [Test]
    public void Add_Test()
    {
      int expected = 5;
      int actual = Calculator.Add(2, 3);
      Assert.AreEqual(expected, actual);
    }
  }
}
```

I dette eksempelet oppretter vi en enkel kalkulator som har en metode for å legge sammen to tall. Deretter bruker vi NUnit-testrammeverket for å opprette en testklasse og en testmetode. Inne i testmetoden definerer vi forventede og faktiske verdier og bruker Assert-metoden til å validere om verdien vi får fra kalkulatoren er riktig.

Det er viktig å merke seg at i eksempelet over, bruker vi TestFixture og Test-attributtene for å markere klassen og metoden som en test. Dette er nødvendig for at testrammeverket skal kunne kjøre testene.

## Dypdykk i skriving av tester

Når du begynner å skrive tester i C#, er det mange ting å vurdere. Du vil kanskje vurdere ulike testdesignmønstre som enhetstesting, integrasjonstesting og aksepttesting. Du må også ta hensyn til hvordan du skal mocks og stubs for å isolere koden din og skrive mer effektive tester.

En ting du også må huske på er å skrive testene før du skriver selve koden. Dette konseptet kalles testdrevet utvikling (TDD) og kan hjelpe deg å tenke mer nøye gjennom koden din og resultere i bedre strukturert kode.

## Se også

- [NUnit dokumentasjon](https://nunit.org/)
- [xUnit dokumentasjon](https://xunit.net/)
- [Introduksjon til enhetstesting i C#](https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-nunit)
- [Hva er TDD?](https://www.agilealliance.org/glossary/tdd/)