---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:18.228086-07:00
description: "Pisanie test\xF3w w C# polega na tworzeniu zautomatyzowanych skrypt\xF3\
  w w celu weryfikacji funkcjonalno\u015Bci kodu, gwarantuj\u0105c, \u017Ce dzia\u0142\
  a on zgodnie z\u2026"
lastmod: '2024-02-25T18:49:33.776544-07:00'
model: gpt-4-0125-preview
summary: "Pisanie test\xF3w w C# polega na tworzeniu zautomatyzowanych skrypt\xF3\
  w w celu weryfikacji funkcjonalno\u015Bci kodu, gwarantuj\u0105c, \u017Ce dzia\u0142\
  a on zgodnie z\u2026"
title: "Pisanie test\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie testów w C# polega na tworzeniu zautomatyzowanych skryptów w celu weryfikacji funkcjonalności kodu, gwarantując, że działa on zgodnie z oczekiwaniami. Programiści wykonują to, aby wyłapywać błędy we wczesnej fazie, ułatwiać refaktoryzację kodu oraz zapewniać, że nowe zmiany nie zakłócają istniejących funkcji, tym samym zwiększając jakość i niezawodność oprogramowania.

## Jak to zrobić:

Deweloperzy C# głównie używają frameworków NUnit lub xUnit do pisania testów ze względu na ich elastyczność i obszerny zestaw funkcjonalności. Oto podstawowy przykład użycia NUnit do testowania prostej funkcji dodawania:

1. **Zainstaluj NUnit i NUnit3TestAdapter** poprzez menedżera pakietów NuGet lub interfejs wiersza poleceń .NET:
```powershell
dotnet add package NUnit
dotnet add package NUnit3TestAdapter
```

2. **Utwórz projekt biblioteki klas C#**, jeśli jeszcze tego nie zrobiłeś.

3. **Napisz prostą funkcję do testowania**. Na przykład metodę dodawania w klasie o nazwie `Calculator`:
```csharp
public class Calculator
{
    public int Add(int a, int b)
    {
        return a + b;
    }
}
```

4. **Napisz klasę testową** używając NUnit:
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

5. **Uruchom test** za pomocą test runnera twojego IDE lub interfejsu wiersza poleceń .NET:
```powershell
dotnet test
```

### Przykładowy wynik:

Zakładając, że twój test przeszedł pomyślnie, powinieneś zobaczyć wynik podobny do tego:
```
Test Run Successful.
Total tests: 1
     Passed: 1
 Total time: 1.2345 Seconds
```

### Użycie xUnit:

Jeśli preferujesz xUnit, konfiguracja jest podobna do NUnit. Oto jak przepisać przykładowy test dla klasy `Calculator` używając xUnit:

1. **Zainstaluj xUnit i xUnit.runner.visualstudio**:
```powershell
dotnet add package xUnit
dotnet add package xUnit.runner.visualstudio
```

2. **Napisz klasę testową używając xUnit**:
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

3. **Uruchom test za pomocą interfejsu wiersza poleceń .NET** lub zintegrowanego test runnera twojego IDE.

Zarówno NUnit, jak i xUnit oferują potężne funkcje do testowania parametryzowanego, operacji ustawienia/zwolnienia zasobów i organizowania testów w kategorie, czyniąc je nieodzownymi narzędziami w zestawie programisty C# do zapewniania jakości kodu i jego funkcjonalności.
