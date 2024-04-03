---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:46.885806-07:00
description: "Hoe: Laten we duiken in wat C# code met NUnit, een populair testraamwerk:\
  \ 1. Stel je testraamwerk in - doorgaans inbegrepen als een NuGet-pakket. 2.\u2026"
lastmod: '2024-03-13T22:44:50.813794-06:00'
model: gpt-4-0125-preview
summary: Laten we duiken in wat C# code met NUnit, een populair testraamwerk.
title: Tests Schrijven
weight: 36
---

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
