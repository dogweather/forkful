---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:13.005481-07:00
description: "Att skriva tester i C# inneb\xE4r att skapa automatiserade skript f\xF6\
  r att validera din kods funktionalitet, s\xE4kerst\xE4lla att den beter sig som\
  \ f\xF6rv\xE4ntat.\u2026"
lastmod: '2024-03-13T22:44:37.916802-06:00'
model: gpt-4-0125-preview
summary: "Att skriva tester i C# inneb\xE4r att skapa automatiserade skript f\xF6\
  r att validera din kods funktionalitet, s\xE4kerst\xE4lla att den beter sig som\
  \ f\xF6rv\xE4ntat."
title: Skriva tester
weight: 36
---

## Hur man gör:
C#-utvecklare använder primärt ramverken NUnit eller xUnit för att skriva tester på grund av deras flexibilitet och omfattande funktionssätt. Här är ett grundläggande exempel som använder NUnit för att testa en enkel additionsfunktion:

1. **Installera NUnit och NUnit3TestAdapter** via NuGet Package Manager eller .NET CLI:
```powershell
dotnet add package NUnit
dotnet add package NUnit3TestAdapter
```

2. **Skapa ett C#-klassbiblioteksprojekt** om du inte redan har gjort det.

3. **Skriv en enkel funktion** att testa. Till exempel en additions metod i en klass som heter `Calculator`:
```csharp
public class Calculator
{
    public int Add(int a, int b)
    {
        return a + b;
    }
}
```

4. **Skriv en testklass** med NUnit:
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
            // Arrange
            var calculator = new Calculator();
            int expected = 5;

            // Act
            int actual = calculator.Add(2, 3);

            // Assert
            Assert.AreEqual(expected, actual);
        }
    }
}
```

5. **Kör testet** med din IDE:s testkörare eller .NET CLI:
```powershell
dotnet test
```

### Exempel på utskrift:
Om ditt test passerar bör du se en utskrift liknande denna:
```
Test Run Successful.
Total tests: 1
     Passed: 1
 Total time: 1.2345 Sekunder
```

### Använda xUnit:
Om du föredrar xUnit är inställningen liknande NUnit. Så här skriver du om testexemplet för `Calculator`-klassen med xUnit:

1. **Installera xUnit och xUnit.runner.visualstudio**:
```powershell
dotnet add package xUnit
dotnet add package xUnit.runner.visualstudio
```

2. **Skriv en testklass med xUnit**:
```csharp
using Xunit;

namespace CalculatorTests
{
    public class CalculatorTests
    {
        [Fact]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // Arrange
            var calculator = new Calculator();
            int expected = 5;

            // Act
            int actual = calculator.Add(2, 3);

            // Assert
            Assert.Equal(expected, actual);
        }
    }
}
```

3. **Kör testet med .NET CLI** eller din IDE:s integrerade testkörare.

Både NUnit och xUnit erbjuder kraftfulla funktioner för parametriserade tester, setup/teardown-operationer och att organisera tester i kategorier, vilket gör dem till oumbärliga verktyg i C#-programmerarens verktygslåda för att säkerställa kodkvalitet och funktionalitet.
