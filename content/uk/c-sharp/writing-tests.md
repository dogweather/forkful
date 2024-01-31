---
title:                "Написання тестів"
date:                  2024-01-19
html_title:           "Arduino: Написання тестів"
simple_title:         "Написання тестів"

category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?

Тести - це перевірки правильності роботи коду. Програмісти пишуть тести, щоб забезпечити надійність програм і полегшити зміни без боязні "поламати" щось.

## Як це робити:

```C#
using NUnit.Framework;

namespace UkrainianApp.Tests
{
    [TestFixture]
    public class CalculatorTests
    {
        [Test]
        public void Add_WhenCalled_ReturnsSumOfArguments()
        {
            var calculator = new Calculator();

            var result = calculator.Add(5, 3);

            Assert.That(result, Is.EqualTo(8)); 
        }
        // Додаємо тест для функції 'Subtract'
        [Test]
        public void Subtract_WhenCalled_ReturnsDifferenceOfArguments()
        {
            var calculator = new Calculator();

            var result = calculator.Subtract(5, 3);

            Assert.That(result, Is.EqualTo(2)); 
        }
    }

    public class Calculator
    {
        public int Add(int a, int b)
        {
            return a + b;
        }

        public int Subtract(int a, int b)
        {
            return a - b;
        }
    }
}
```
Виконайте тести, результат повинен виглядати так:
```
Test Name:  Add_WhenCalled_ReturnsSumOfArguments
   Status: Passed

Test Name:  Subtract_WhenCalled_ReturnsDifferenceOfArguments
   Status: Passed
```

## Поглиблено:

Історично, тести розробляли для підвищення довіри до коду. Використання TDD (Test-Driven Development) почало набирати популярність у 2000-х. Альтернативи NUnit включають MSTest і xUnit. Головне — розділити бізнес-логіку від інтерфейсу, щоб тести були простішими та швидшими.

## Джерела для ознайомлення:

- [NUnit Documentation](https://docs.nunit.org/)
- [Microsoft Docs on Testing in .NET](https://docs.microsoft.com/en-us/dotnet/core/testing/)
- [Test-Driven Development by Example (book by Kent Beck)](https://www.goodreads.com/book/show/387190.Test_Driven_Development)
