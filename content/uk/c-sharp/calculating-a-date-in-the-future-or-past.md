---
title:                "Обчислення дати в майбутньому або минулому"
html_title:           "C#: Обчислення дати в майбутньому або минулому"
simple_title:         "Обчислення дати в майбутньому або минулому"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що і Навіщо?

Обчислення дати в майбутньому або минулому - це визначення точного дня, місяця і року на відстані 'x' днів від заданої дати. Програмісти роблять це, щоб виконувати розрахунки, що вимагають дати, наприклад, обчислення термінів платежів або відстеження строків доставки.

## Як це робиться:

Приклад коду C# для вирішення цього завдання:

```C#
using System;

class Program {
    static void Main() {
        DateTime today = DateTime.Today;
        Console.WriteLine("Сьогодні: {0}", today);

        DateTime past = today.AddDays(-90);
        Console.WriteLine("90 днів назад: {0}", past);

        DateTime future = today.AddDays(90);
        Console.WriteLine("Через 90 днів: {0}", future);
    }
}
```

Виведення у консолі:
```C#
Сьогодні: 2022-04-12 00:00:00
90 днів назад: 2022-01-12 00:00:00
Через 90 днів: 2022-07-11 00:00:00
```
## Занурення глибше:

1. **Історичний контекст**: Дати і робота з часом завжди були частиною програмування. Незалежно від того, чи розробляєте ви системы, мобільні додатки, веб-сайти або ігри, ви, як правило, стикнетеся з потребою обчислювати дати в майбутньому або минулому.
2. **Альтернативи**: .NET приймає низку надбудов на базову систему DateTime, що пропонують більше можливостей для праці з датами і часом, такі як NodaTime або DateNet.
3. **Деталі реалізації**: Система DateTime в .NET користується структурою DateTime, яка зберігає дату і час як число "тиків" (на десятотисячну долю секунди), що минуло з моменту 0001-01-01T00:00:00.000. Використовуючи метод `AddDays()`, ми додаємо відповідну кількість "тиків" до поточного числа.

## Подивіться також:

- [System.DateTime в .NET](https://docs.microsoft.com/uk-ua/dotnet/api/system.datetime?view=net-6.0)
- [Робота з датами і часом в C#](https://code-maze.com/datetime-csharp/)
- [NodaTime](https://nodatime.org/)
- [DateNet on GitHub](https://github.com/Galarius/DateNet)