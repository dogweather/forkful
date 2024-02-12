---
title:                "Отримання поточної дати"
aliases:
- uk/c-sharp/getting-the-current-date.md
date:                  2024-02-03T19:09:38.493604-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отримання поточної дати"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Отримання поточної дати в C# передбачає отримання даних про поточну дату та час від системи. Програмісти часто потребують доступу до цієї інформації для ведення журналів, створення міток часу операцій або планування завдань у додатках, гарантуючи, що дії здійснюються у точно визначений час та дані маркуються точними мітками часу.

## Як:
C# надає простий спосіб отримання поточної дати за допомогою класу `DateTime`, який є частиною простору імен System .NET Framework. Приклад нижче демонструє, як отримати поточну дату, і за бажанням, час.

```csharp
using System;

class Program
{
    static void Main()
    {
        // Отримує лише поточну дату
        DateTime currentDate = DateTime.Today;
        Console.WriteLine(currentDate.ToString("d"));  // Вивід: MM/dd/yyyy
        
        // Отримує поточну дату та час
        DateTime currentDateTime = DateTime.Now;
        Console.WriteLine(currentDateTime.ToString()); // Вивід: MM/dd/yyyy HH:mm:ss

        // Отрімує поточную дату та час у UTC
        DateTime currentUtcDateTime = DateTime.UtcNow;
        Console.WriteLine(currentUtcDateTime.ToString()); // Вивід: MM/dd/yyyy HH:mm:ss
    }
}
```

Що стосується бібліотек третіх сторін, NodaTime пропонує надійну альтернативу для маніпуляції датою та часом, включаючи отримання поточної дати в різних календарях і часових зонах.

```csharp
using NodaTime;
using System;

class Program
{
    static void Main()
    {
        // Використання NodaTime для отримання поточної дати в ISO календарі
        LocalDate currentDate = SystemClock.Instance.GetCurrentInstant().InUtc().Date;
        Console.WriteLine(currentDate.ToString()); // Вивід: yyyy-MM-dd

        // Для дат, специфічних для часової зони
        DateTimeZone zone = DateTimeZoneProviders.Tzdb["America/New_York"];
        LocalDate currentZonedDate = SystemClock.Instance.GetCurrentInstant().InZone(zone).Date;
        Console.WriteLine(currentZonedDate.ToString()); // Вивід: yyyy-MM-dd
    }
}
```

Це демонструє базове використання вбудованого класу `DateTime` та розширені можливості, які надає NodaTime, особливо корисні для додатків, які потребують обробки різних часових зон або систем календарів.
