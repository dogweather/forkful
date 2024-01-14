---
title:    "C#: Друк вихідних даних для відладки"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Чому

Кожен програміст хоч раз у житті стикається з проблемою пошуку і виправлення помилок у своїй програмі. Часом це може бути дуже складно, особливо коли проблема знаходиться десь в глибоках коду. Але єдиний спосіб зрозуміти, що відбувається в програмі в цьому випадку - це використовувати виведення налагоджування (debug output).

## Як використовувати виведення налагоджування

Є кілька способів використання виведення налагоджування в C#. Перший спосіб - використовувати метод `Console.WriteLine()`:

```C#
string message = "Hello world!";
Console.WriteLine(message);
```
Ви побачите виведений текст в консолі під час виконання програми.

Також ви можете використовувати метод `Debug.WriteLine()` з простору імен `System.Diagnostics`:

```C#
Debug.WriteLine(message);
```

Цей метод також виведе повідомлення у вікні виведення налагоджування у вашій інтегрованій середовищі розробки (IDE).

## Глибоко пояснення про виведення налагоджування

Використання виведення налагоджування дозволяє вам перевіряти значення змінних та умов запуску коду в реальному часі. Ви можете додавати виведення налагоджування в різних місцях свого коду, щоб перевірити, чи виконується певна умова, чи є значення змінної вірним, тощо.

Також, ви можете використовувати виведення налагоджування для вивчення, як ті чи інші процеси відбуваються в вашій програмі. Це може бути корисно при пошуку та виправленні проблем у великих та складних програмах.

## Дивись також

- [How to Debug Your Code in C#](https://www.tutorialspoint.com/csharp/csharp_debugging.htm)
- [Debug.WriteLine Method (System.Diagnostics)](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug.writeline?view=netframework-4.7.2)
- [Writing Output to the Debug Window in Visual Studio](https://docs.microsoft.com/en-us/visualstudio/debugger/writing-output-to-the-debug-window)