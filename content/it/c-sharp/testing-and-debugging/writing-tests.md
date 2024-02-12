---
title:                "Scrivere test"
date:                  2024-02-03T19:30:13.809699-07:00
model:                 gpt-4-0125-preview
simple_title:         "Scrivere test"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Scrivere test in C# comporta la creazione di script automatizzati per validare la funzionalità del proprio codice, assicurando che si comporti come previsto. I programmatori lo fanno per individuare i bug precocemente, facilitare il refactoring del codice, e garantire che le nuove modifiche non compromettano le funzioni esistenti, aumentando così la qualità e l'affidabilità del software.

## Come fare:

Gli sviluppatori C# usano principalmente i framework NUnit o xUnit per scrivere test grazie alla loro flessibilità e ampio set di funzionalità. Ecco un esempio base che utilizza NUnit per testare una semplice funzione di addizione:

1. **Installa NUnit e NUnit3TestAdapter** tramite NuGet Package Manager o il .NET CLI:
```powershell
dotnet add package NUnit
dotnet add package NUnit3TestAdapter
```

2. **Crea un progetto di class library C#** se non lo hai già fatto.

3. **Scrivi una semplice funzione** da testare. Per esempio, un metodo di addizione in una classe denominata `Calculator`:
```csharp
public class Calculator
{
    public int Add(int a, int b)
    {
        return a + b;
    }
}
```

4. **Scrivi una classe di test** usando NUnit:
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

5. **Esegui il test** usando il runner di test del tuo IDE o il .NET CLI:
```powershell
dotnet test
```

### Output di esempio:

Assumendo che il tuo test sia superato, dovresti vedere un output simile a questo:
```
Test Run Successful.
Total tests: 1
     Passed: 1
 Total time: 1.2345 Seconds
```

### Usando xUnit:

Se preferisci xUnit, la configurazione è simile a quella di NUnit. Ecco come riscriveresti l'esempio di test per la classe `Calculator` usando xUnit:

1. **Installa xUnit e xUnit.runner.visualstudio**:
```powershell
dotnet add package xUnit
dotnet add package xUnit.runner.visualstudio
```

2. **Scrivi una classe di test usando xUnit**:
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

3. **Esegui il test usando il .NET CLI** o il runner di test integrato del tuo IDE.

Sia NUnit che xUnit offrono potenti funzionalità per test parametrizzati, operazioni di setup/teardown, e organizzazione dei test in categorie, rendendoli strumenti indispensabili nel kit dello sviluppatore C# per garantire la qualità e la funzionalità del codice.
