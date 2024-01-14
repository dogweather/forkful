---
title:                "C#: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Perché scrivere test in C#
Scrivere test per il proprio codice è fondamentale per garantire che funzioni correttamente e senza errori. Inoltre, aiuta a identificare e risolvere eventuali problemi in modo più efficiente.

## Come scrivere test in C#
Per scrivere test in C#, è necessario utilizzare un framework di testing come NUnit o xUnit. Di seguito un esempio di codice che utilizza NUnit per testare una funzione che calcola il doppio di un numero intero:

```C#
[Test]
public void TestDouble()
{
    int result = Calculator.Double(5);
    Assert.AreEqual(10, result);
}
```
Output:
```
NUnit.Framework.AssertionException:   Expected: 10
  But was:  8
```

## Approfondimento sui test in C#
Esistono diversi tipi di test che possono essere scritti in C#, come test di unità, di integrazione e di accettazione. È importante comprendere la differenza tra di essi e quando utilizzarli per garantire una copertura completa del codice.

Inoltre, è possibile utilizzare strumenti di code coverage per verificare quanto del proprio codice è testato e assicurarsi di avere una buona percentuale di copertura.

## Vedi anche
- [Introduzione a NUnit](https://docs.microsoft.com/it-it/dotnet/core/testing/unit-testing-with-nunit)
- [Guida a xUnit.net](https://xunit.net/)
- [Code coverage in Visual Studio](https://docs.microsoft.com/it-it/visualstudio/code-quality/using-code-coverage-to-determine-how-much-code-is-being-tested)