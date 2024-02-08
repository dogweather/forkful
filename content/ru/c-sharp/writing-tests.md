---
title:                "Написание тестов"
aliases:
- ru/c-sharp/writing-tests.md
date:                  2024-01-29T00:05:36.176723-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написание тестов"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c-sharp/writing-tests.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Написание тестов в программировании означает создание мини-программ для проверки работы вашего кода как ожидается. Программисты делают это для поиска ошибок, обеспечения качества и экономии времени на исправление проблем позже.

## Как это сделать:
Давайте рассмотрим некоторый код на C# с использованием NUnit, популярного фреймворка для тестирования:

1. Настройте вашу среду для тестирования - обычно включается как пакет NuGet.
2. Напишите тест для простой функции.

Вот быстрый пример теста для метода `Sum`:

```C#
using NUnit.Framework;

namespace CalculatorTests {
    public class Calculator {
        public int Sum(int a, int b) {
            return a + b;
        }
    }

    [TestFixture]
    public class CalculatorTests {
        [Test]
        public void TestSum() {
            var calculator = new Calculator();
            var result = calculator.Sum(2, 3);
            Assert.AreEqual(5, result);
        }
    }
}
```

Запустите тест. Если он пройден, вы увидите:

```
Test Passed
```

В противном случае вы получите детали о том, почему он не пройден.

## Погружение глубже
Модульное тестирование развивалось с 1970-х годов. Заметные достижения включают разработку, управляемую тестами, и автоматизированные фреймворки для тестирования. Для C#, MSTest и xUnit являются надежными альтернативами NUnit. Основные моменты включают в себя:

1. **Исторический контекст**: Кент Бек среди прочих разработал архитектуру xUnit, которая лежит в основе многих фреймворков.
2. **Альтернативы**: MSTest является родным фреймворком тестирования от Microsoft, в то время как xUnit - это бесплатный инструмент с открытым исходным кодом.
3. **Детали реализации**: Тесты должны быть изолированными, повторяемыми и быстрыми. Запускайте их как часть вашего процесса сборки.

## Смотрите также
- [Документация NUnit](https://docs.nunit.org/)
- [Обзор тестирования Microsoft](https://docs.microsoft.com/ru-ru/dotnet/core/testing/)
- [GitHub xUnit](https://github.com/xunit/xunit)
