---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:22.208768-07:00
description: "\xC5 skrive tester i C# inneb\xE6rer \xE5 lage automatiserte skript\
  \ for \xE5 validere funksjonaliteten til koden din, for \xE5 sikre at den oppf\xF8\
  rer seg som forventet.\u2026"
lastmod: '2024-03-13T22:44:40.799318-06:00'
model: gpt-4-0125-preview
summary: "\xC5 skrive tester i C# inneb\xE6rer \xE5 lage automatiserte skript for\
  \ \xE5 validere funksjonaliteten til koden din, for \xE5 sikre at den oppf\xF8rer\
  \ seg som forventet.\u2026"
title: Skrive tester
weight: 36
---

## Hva & Hvorfor?

Å skrive tester i C# innebærer å lage automatiserte skript for å validere funksjonaliteten til koden din, for å sikre at den oppfører seg som forventet. Programmerere gjør dette for å fange opp feil tidlig, legge til rette for koderestrukturering, og sikre at nye endringer ikke ødelegger eksisterende funksjoner, noe som øker programvarens kvalitet og pålitelighet.

## Hvordan:

C#-utviklere bruker primært NUnit- eller xUnit-rammeverkene for å skrive tester på grunn av deres fleksibilitet og omfattende funksjonssett. Her er et grunnleggende eksempel som bruker NUnit for å teste en enkel addisjonsfunksjon:

1. **Installer NUnit og NUnit3TestAdapter** via NuGet Package Manager eller .NET CLI:
```powershell
dotnet add package NUnit
dotnet add package NUnit3TestAdapter
```

2. **Opprett et C# klassebibliotekprosjekt** hvis du ikke allerede har gjort det.

3. **Skriv en enkel funksjon** for å teste. For eksempel, en addisjonsmetode i en klasse med navn `Calculator`:
```csharp
public class Calculator
{
    public int Add(int a, int b)
    {
        return a + b;
    }
}
```

4. **Skriv en testklasse** ved bruk av NUnit:
```csharp
using NUnit.Framework;

namespace CalculatorTests
{
    [TestFixture]
    public class CalculatorTests
    {
        [Test]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // Arrange (Forbered)
            var calculator = new Calculator();
            int expected = 5;

            // Act (Utfør)
            int actual = calculator.Add(2, 3);

            // Assert (Bekreft)
            Assert.AreEqual(expected, actual);
        }
    }
}
```

5. **Kjør testen** ved bruk av din IDEs testkjører eller .NET CLI:
```powershell
dotnet test
```

### Eksempel på utdata:

Forutsatt at testen din passerer, bør du se en utdata som ligner på dette:
```
Test Run Successful.
Total tests: 1
     Passed: 1
 Total time: 1.2345 Seconds
```

### Bruk av xUnit:

Hvis du foretrekker xUnit, er oppsettet lignende som for NUnit. Her er hvordan du skriver om testeksemplet for `Calculator`-klassen ved bruk av xUnit:

1. **Installer xUnit og xUnit.runner.visualstudio**:
```powershell
dotnet add package xUnit
dotnet add package xUnit.runner.visualstudio
```

2. **Skriv en testklasse ved bruk av xUnit**:
```csharp
using Xunit;

namespace CalculatorTests
{
    public class CalculatorTests
    {
        [Fact]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // Arrange (Forbered)
            var calculator = new Calculator();
            int expected = 5;

            // Act (Utfør)
            int actual = calculator.Add(2, 3);

            // Assert (Bekreft)
            Assert.Equal(expected, actual);
        }
    }
}
```

3. **Kjør testen ved hjelp av .NET CLI** eller din IDEs integrerte testkjører.

Både NUnit og xUnit tilbyr kraftige funksjoner for parametrisert testing, oppsett/nedrivningsoperasjoner, og organisering av tester i kategorier, noe som gjør dem til uunnværlige verktøy i C#-programmererens verktøykasse for å sikre kodekvalitet og funksjonalitet.
