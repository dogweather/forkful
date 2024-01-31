---
title:                "Pisanie testów"
date:                  2024-01-19
html_title:           "Bash: Pisanie testów"
simple_title:         "Pisanie testów"

category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Pisanie testów to proces tworzenia kodu, który automatycznie weryfikuje działanie innego kodu. Programiści robią to, by zapewnić poprawność działania programów, ułatwić późniejsze zmiany w kodzie oraz szybciej wykrywać błędy.

## How to: (Jak to zrobić?)
```C#
using System;
using Xunit; // Wykorzystujemy framework xUnit

public class Calculator
{
    public int Add(int number1, int number2)
    {
        return number1 + number2;
    }
}

public class CalculatorTests
{
    [Fact]
    public void Add_ShouldCalculateCorrectSum()
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
```
Po uruchomieniu testu, otrzymamy wynik pokazujący, czy suma jest wyliczona prawidłowo.

## Deep Dive (Szczegółowa analiza)
Historia testowania oprogramowania rozpoczęła się niemalże z początkiem pisania programów. Pierwsze testy były ręczne i powolne, ale z czasem powstały narzędzia automate testowania, takie jak JUnit dla Javy czy NUnit i xUnit dla C#. Alternatywne podejścia to TDD (Test-Driven Development), gdzie najpierw piszemy testy, a potem kod, oraz BDD (Behavior-Driven Development), które koncentruje się na zachowaniu. Ważne jest też zrozumienie różnicy pomiędzy testami jednostkowymi, integracyjnymi i systemowymi, które sprawdzają różne aspekty aplikacji.

## See Also (Zobacz też)
- [xUnit.net Documentation](https://xunit.net/docs/getting-started/netfx/visual-studio)
- [Microsoft Testowanie jednostkowe w C#](https://docs.microsoft.com/pl-pl/dotnet/core/testing/unit-testing-with-dotnet-test)
- [Martin Fowler on Unit Testing](https://martinfowler.com/bliki/UnitTest.html)
- [Test-Driven Development (TDD)](https://www.agilealliance.org/glossary/tdd/)
