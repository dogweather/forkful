---
title:    "C#: Обчислення дати в майбутньому чи минулому"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

Багато людей можуть задатися питанням, чому нам потрібно обчислювати дату в майбутньому або в минулому. Одна з причин - це зручність планування подій або визначення термінів. Наприклад, якщо ви плануєте відпустку, вам буде потрібно знати дату початку та кінця, щоб придбати квитки та забронювати готель. Також ви можете використовувати цю функцію для обчислення терміну повернення позики або інших фінансових обов'язків.

## Як

```C#
using System;

public class Program
{
    public static void Main()
    {
        // Обчислення дати у майбутньому
        DateTime today = DateTime.Today;
        DateTime futureDate = today.AddDays(30);
        Console.WriteLine("Дата через 30 днів: " + futureDate);

        // Обчислення дати у минулому
        DateTime pastDate = today.AddDays(-30);
        Console.WriteLine("Дата 30 днів назад: " + pastDate);
    }
}

// Вихідні дані:
// Дата через 30 днів: 1/24/2021 12:00:00 AM
// Дата 30 днів назад: 12/25/2020 12:00:00 AM
```

У цьому прикладі ми використовуємо клас `DateTime` та його метод `AddDays()` для обчислення дати у майбутньому або минулому. Ми передаємо кількість днів, на яку ми хочемо змінити поточну дату. Крім методу `AddDays()`, існує також методи `AddMonths()` та `AddYears()`, які дозволяють додати місяці або роки до поточної дати.

## Deep Dive

Щоб розуміти, як функція `AddDays()` працює, потрібно зрозуміти, що таке `DateTime`. Це структура даних, яка представляє дату та час в .NET. Вона містить властивості, такі як `Day`, `Month` та `Year`, та методи, такі як `AddDays()`, `AddMonths()` та `AddYears()`.

Коли ми викликаємо метод `AddDays()`, ми насправді створюємо новий об'єкт `DateTime`, який повертається як результат. Оригінальний об'єкт `DateTime` залишається незмінним. Це є важливим аспектом при роботі з датами, оскільки вони є незмінними.

## Дивись також

- [Microsoft Docs: DateTime структура](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [W3Schools: C# DateTime](https://www.w3schools.com/cs/cs_dates.asp)
- [C# AddDays(), AddMonths(), AddYears()](https://www.c-sharpcorner.com/article/c-sharp-adddays-addmonths-addyears-method/)