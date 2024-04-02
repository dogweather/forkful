---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:44.331353-07:00
description: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u0441\
  \u0442\u0456\u0432 \u043D\u0430 C# \u043F\u043E\u043B\u044F\u0433\u0430\u0454 \u0443\
  \ \u0441\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u0456 \u0430\u0432\u0442\u043E\
  \u043C\u0430\u0442\u0438\u0437\u043E\u0432\u0430\u043D\u0438\u0445 \u0441\u043A\u0440\
  \u0438\u043F\u0442\u0456\u0432 \u0434\u043B\u044F \u043F\u0435\u0440\u0435\u0432\
  \u0456\u0440\u043A\u0438 \u0444\u0443\u043D\u043A\u0446\u0456\u043E\u043D\u0430\u043B\
  \u044C\u043D\u043E\u0441\u0442\u0456 \u0432\u0430\u0448\u043E\u0433\u043E \u043A\
  \u043E\u0434\u0443, \u0437\u0430\u0431\u0435\u0437\u043F\u0435\u0447\u0435\u043D\
  \u043D\u044F \u0439\u043E\u0433\u043E \u043E\u0447\u0456\u043A\u0443\u0432\u0430\
  \u043D\u043E\u0457 \u043F\u043E\u0432\u0435\u0434\u0456\u043D\u043A\u0438.\u2026"
lastmod: '2024-03-13T22:44:49.295119-06:00'
model: gpt-4-0125-preview
summary: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u0441\
  \u0442\u0456\u0432 \u043D\u0430 C# \u043F\u043E\u043B\u044F\u0433\u0430\u0454 \u0443\
  \ \u0441\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u0456 \u0430\u0432\u0442\u043E\
  \u043C\u0430\u0442\u0438\u0437\u043E\u0432\u0430\u043D\u0438\u0445 \u0441\u043A\u0440\
  \u0438\u043F\u0442\u0456\u0432 \u0434\u043B\u044F \u043F\u0435\u0440\u0435\u0432\
  \u0456\u0440\u043A\u0438 \u0444\u0443\u043D\u043A\u0446\u0456\u043E\u043D\u0430\u043B\
  \u044C\u043D\u043E\u0441\u0442\u0456 \u0432\u0430\u0448\u043E\u0433\u043E \u043A\
  \u043E\u0434\u0443, \u0437\u0430\u0431\u0435\u0437\u043F\u0435\u0447\u0435\u043D\
  \u043D\u044F \u0439\u043E\u0433\u043E \u043E\u0447\u0456\u043A\u0443\u0432\u0430\
  \u043D\u043E\u0457 \u043F\u043E\u0432\u0435\u0434\u0456\u043D\u043A\u0438.\u2026"
title: "\u041F\u0438\u0441\u044C\u043C\u043E \u0442\u0435\u0441\u0442\u0456\u0432"
weight: 36
---

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
