---
title:                "Scrivere test"
date:                  2024-01-19
simple_title:         "Scrivere test"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere test significa codificare situazioni specifiche per verificare la nostra applicazione. Si fa per prevenire errori, garantire la qualità del codice e facilitare le modifiche future.

## How to:
In C#, i test possono essere scritti usando NUnit o xUnit. Di seguito un esempio con xUnit:

```C#
using Xunit;

public class CalculatorTests
{
    [Fact]
    public void CanAdd()
    {
        var calculator = new Calculator();
        Assert.Equal(4, calculator.Add(2, 2));
    }
}

public class Calculator
{
    public int Add(int a, int b)
    {
        return a + b;
    }
}
```

Output di test riuscito: 
```
Test Passed: CanAdd
```

## Deep Dive
I test in C# hanno radici nel framework NUnit, ispirato da JUnit e introdotto nei primi anni 2000. Alternative includono MSTest, incluso in Visual Studio, e xUnit, una scelta moderna popolare per la sua sintassi semplificata e funzionalità avanzate. Quando si scrivono test, è importante considerare cosa testare (logica di business) e cosa evitare (parti del codice dipendenti da fattori esterni).

## See Also
- xUnit official documentation: https://xunit.net/
- NUnit official documentation: https://nunit.org/
- Microsoft guide to unit testing: https://docs.microsoft.com/en-us/dotnet/core/testing/
