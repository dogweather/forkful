---
title:                "Обработка ошибок"
date:                  2024-01-28T23:58:58.870048-07:00
model:                 gpt-4-0125-preview
simple_title:         "Обработка ошибок"
programming_language: "C#"
category:             "C#"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c-sharp/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Обработка ошибок в C# — это управление неожиданностями, подобно спотыканию о шнурки. Программы могут "спотыкаться" о некорректные данные или нестабильные соединения. Мы обрабатываем ошибки, чтобы наше программное обеспечение не "упало носом", позволяя ему изящно восстановиться.

## Как это сделать:

Начнем с блока try-catch. Это как поставить сеть безопасности под канатоходца. Если он поскользнется, он не упадет вниз — его поймают.

```C#
using System;

class ПримерОбработкиОшибок {
    static void Main() {
        try {
            int[] числа = {1, 2, 3};
            Console.WriteLine(числа[5]);  // Ой, индекс вне границ!
        } catch (IndexOutOfRangeException e) {
            Console.WriteLine("Ошибка перехвачена: " + e.Message);
        }
    }
}
```

Пример вывода при возникновении ошибок:
```
Ошибка перехвачена: Индекс находился вне границ массива.
```

Теперь добавим блок finally — это то, что происходит в любом случае, как и платежи налогов.

```C#
try {
    // Потенциально проблемный код здесь
} catch (SomeSpecificException e) {
    // Обработка этой конкретной ошибки здесь
} finally {
    // Этот код выполняется независимо от того, что произошло выше
    Console.WriteLine("Это всегда выполняется.");
}
```

## Глубокое погружение

Обработка ошибок была в C# с момента его создания. Со временем она эволюционировала. В прошлом программисты полагались на возвращаемые коды или глобальные флаги для сигнализации о проблемах — громоздко и склонно к ошибкам.

C# использует исключения, более современный подход. Исключение генерируется, когда происходит что-то неожиданное, подобно поднятию флага на игре в американский футбол. Структурированная обработка исключений с блоками try, catch и finally делает управление этими моментами более ясным и чистым, чем старые методы проверки ошибок.

Альтернативы? Конечно. Есть `UnhandledExceptionEventHandler` для исключений, которые прошли мимо. Или в асинхронном коде, обработка ошибок делается немного по-другому с объектами `Task`, которые несут собственный груз исключений.

Детали реализации — подобно мелкому шрифту — важны. Исключения могут быть дорогостоящими, тормозя производительность, если их бросать без разбора. Поэтому мы используем их для исключительных случаев, а не для управления логикой каждый день.

## См. также

- [Официальная документация по исключениям в C#](https://docs.microsoft.com/en-us/dotnet/csharp/fundamentals/exceptions/exception-handling)
- [Лучшие практики обработки исключений в C#](https://docs.microsoft.com/en-us/dotnet/standard/exceptions/best-practices-for-exceptions)