---
title:                "Tests Schrijven"
aliases: - /nl/c-sharp/writing-tests.md
date:                  2024-01-28T22:12:46.885806-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tests Schrijven"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c-sharp/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Tests schrijven in programmeren betekent het creëren van mini-programma's om te controleren of je code werkt zoals verwacht. Programmeurs doen dit om bugs te vinden, kwaliteit te waarborgen, en tijd te besparen bij het later oplossen van problemen.

## Hoe:
Laten we duiken in wat C# code met NUnit, een populair testraamwerk:

1. Stel je testraamwerk in - doorgaans inbegrepen als een NuGet-pakket.
2. Schrijf een test voor een eenvoudige functie.

Hier is een snel voorbeeld van een test voor een `Sum` methode:

```C#
using NUnit.Framework;

namespace CalculatorTests {
    public class Calculator {
        public int Sum(int a, int b) {
            return a + b;
        }
    }

    [TestFixture]
    public class CalculatorTests {
        [Test]
        public void TestSum() {
            var calculator = new Calculator();
            var resultaat = calculator.Sum(2, 3);
            Assert.AreEqual(5, resultaat);
        }
    }
}
```

Voer de test uit. Als het slaagt, zul je zien:

```
Test Geslaagd
```

Anders krijg je details over waarom het is mislukt.

## Diepgaande Duik
Unittesten is geëvolueerd sinds de jaren '70. Opmerkelijke vooruitgangen omvatten test-gedreven ontwikkeling en geautomatiseerde testraamwerken. Voor C#, zijn MSTest en xUnit solide alternatieven voor NUnit. Belangrijke punten zijn:

1. **Historische Context**: Kent Beck, onder anderen, ontwikkelde de xUnit-architectuur die veel raamwerken ondersteunt.
2. **Alternatieven**: MSTest is het native testraamwerk van Microsoft, terwijl xUnit een gratis, open-source tool is.
3. **Implementatiedetails**: Tests moeten geïsoleerd, herhaalbaar en snel zijn. Draai ze als onderdeel van je bouwproces.

## Zie Ook
- [NUnit Documentatie](https://docs.nunit.org/)
- [Microsoft Testoverzicht](https://docs.microsoft.com/en-us/dotnet/core/testing/)
- [xUnit GitHub](https://github.com/xunit/xunit)
