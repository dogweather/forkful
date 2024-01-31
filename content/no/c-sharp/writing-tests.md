---
title:                "Skriving av tester"
date:                  2024-01-19
html_title:           "Arduino: Skriving av tester"
simple_title:         "Skriving av tester"

category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skriving av tester er prosessen med å sjekke at kode gjør det den skal. Programmerere tester for å fange feil tidlig, sikre kvalitet og forenkle fremtidige endringer.

## Hvordan gjøre det:
C# tilbyr et rammeverk kalt NUnit for å skrive og kjøre tester. Her er et grunnleggende eksempel:

```c#
using NUnit.Framework;

namespace MyAwesomeApp.Tests
{
    [TestFixture]
    public class CalculatorTests
    {
        [Test]
        public void Add_WhenCalled_ReturnSum()
        {
            // Arrange
            var calculator = new Calculator();

            // Act
            var result = calculator.Add(5, 6);

            // Assert
            Assert.AreEqual(11, result);
        }
    }
    
    public class Calculator
    {
        public int Add(int a, int b)
        {
            return a + b;
        }
    }
}
```

Kjør testen og forvent noe som dette som utskrift:

```
Test Name:  Add_WhenCalled_ReturnSum
Test FullName:  MyAwesomeApp.Tests.CalculatorTests.Add_WhenCalled_ReturnSum
Test Outcome: Passed
```

## Dypdykk:
Tester skyter fart på 2000-tallet med agile metodikker som TDD (Test-Drevet Utvikling). Alternativer til NUnit inkluderer xUnit og MSTest. Viktige konsepter i testskriving inkluderer "Arrange-Act-Assert" mønsteret, Mocking og Continuous Integration (CI).

## Se også:
- NUnit offisiell side: [nunit.org](https://nunit.org/)
- xUnit offisiell side: [xunit.net](https://xunit.net/)
- Microsofts guide til testing med MSTest: [docs.microsoft.com/en-us/dotnet/core/testing/](https://docs.microsoft.com/en-us/dotnet/core/testing/)
