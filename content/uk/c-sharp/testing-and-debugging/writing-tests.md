---
title:                "Письмо тестів"
aliases:
- /uk/c-sharp/writing-tests.md
date:                  2024-02-03T19:30:44.331353-07:00
model:                 gpt-4-0125-preview
simple_title:         "Письмо тестів"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Написання тестів на C# полягає у створенні автоматизованих скриптів для перевірки функціональності вашого коду, забезпечення його очікуваної поведінки. Програмісти роблять це, щоб виявити помилки на ранньому етапі, спростити рефакторинг коду та забезпечити, щоб нові зміни не порушили існуючі функції, тим самим підвищуючи якість і надійність програмного забезпечення.

## Як:

Розробники C# зазвичай використовують фреймворки NUnit або xUnit для написання тестів через їхню гнучкість і широкий набір функцій. Ось базовий приклад використання NUnit для тестування простої функції додавання:

1. **Встановіть NUnit і NUnit3TestAdapter** через менеджер пакетів NuGet або .NET CLI:
```powershell
dotnet add package NUnit
dotnet add package NUnit3TestAdapter
```

2. **Створіть проект бібліотеки класів C#**, якщо ви ще цього не зробили.

3. **Напишіть просту функцію** для тестування. Наприклад, метод додавання в класі `Calculator`:
```csharp
public class Calculator
{
    public int Add(int a, int b)
    {
        return a + b;
    }
}
```

4. **Напишіть тестовий клас** використовуючи NUnit:
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
            // Підготовка
            var calculator = new Calculator();
            int expected = 5;

            // Дія
            int actual = calculator.Add(2, 3);

            // Перевірка
            Assert.AreEqual(expected, actual);
        }
    }
}
```

5. **Запустіть тест** використовуючи тестовий раннер вашої ІDE або .NET CLI:
```powershell
dotnet test
```

### Приклад виводу:

Припускаючи, що ваш тест пройшов, ви повинні побачити подібний вивід:
```
Test Run Successful.
Total tests: 1
     Passed: 1
 Total time: 1.2345 Seconds
```

### Використання xUnit:

Якщо ви віддаєте перевагу xUnit, налаштування схоже на NUnit. Ось як ви б переписали приклад тесту для класу `Calculator` використовуючи xUnit:

1. **Встановіть xUnit і xUnit.runner.visualstudio**:
```powershell
dotnet add package xUnit
dotnet add package xUnit.runner.visualstudio
```

2. **Напишіть тестовий клас використовуючи xUnit**:
```csharp
using Xunit;

namespace CalculatorTests
{
    public class CalculatorTests
    {
        [Fact]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // Підготовка
            var calculator = new Calculator();
            int expected = 5;

            // Дія
            int actual = calculator.Add(2, 3);

            // Перевірка
            Assert.Equal(expected, actual);
        }
    }
}
```

3. **Запустіть тест використовуючи .NET CLI** або інтегрований тестовий раннер вашої ІDE.

Як NUnit, так і xUnit надають потужні можливості для параметризованого тестування, налаштувань для підготовки/завершення тестів та організації тестів у категорії, роблячи їх незамінними інструментами в наборі інструментів програміста C# для забезпечення якості коду та його функціональності.
