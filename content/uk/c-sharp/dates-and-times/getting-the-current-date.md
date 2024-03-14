---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:38.493604-07:00
description: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\
  \u043E\u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438 \u0432 C# \u043F\u0435\u0440\
  \u0435\u0434\u0431\u0430\u0447\u0430\u0454 \u043E\u0442\u0440\u0438\u043C\u0430\u043D\
  \u043D\u044F \u0434\u0430\u043D\u0438\u0445 \u043F\u0440\u043E \u043F\u043E\u0442\
  \u043E\u0447\u043D\u0443 \u0434\u0430\u0442\u0443 \u0442\u0430 \u0447\u0430\u0441\
  \ \u0432\u0456\u0434 \u0441\u0438\u0441\u0442\u0435\u043C\u0438. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0447\u0430\u0441\u0442\u043E\
  \ \u043F\u043E\u0442\u0440\u0435\u0431\u0443\u044E\u0442\u044C \u0434\u043E\u0441\
  \u0442\u0443\u043F\u0443 \u0434\u043E \u0446\u0456\u0454\u0457 \u0456\u043D\u0444\
  \u043E\u0440\u043C\u0430\u0446\u0456\u0457 \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:49.306836-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\
  \u043E\u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438 \u0432 C# \u043F\u0435\u0440\
  \u0435\u0434\u0431\u0430\u0447\u0430\u0454 \u043E\u0442\u0440\u0438\u043C\u0430\u043D\
  \u043D\u044F \u0434\u0430\u043D\u0438\u0445 \u043F\u0440\u043E \u043F\u043E\u0442\
  \u043E\u0447\u043D\u0443 \u0434\u0430\u0442\u0443 \u0442\u0430 \u0447\u0430\u0441\
  \ \u0432\u0456\u0434 \u0441\u0438\u0441\u0442\u0435\u043C\u0438. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0447\u0430\u0441\u0442\u043E\
  \ \u043F\u043E\u0442\u0440\u0435\u0431\u0443\u044E\u0442\u044C \u0434\u043E\u0441\
  \u0442\u0443\u043F\u0443 \u0434\u043E \u0446\u0456\u0454\u0457 \u0456\u043D\u0444\
  \u043E\u0440\u043C\u0430\u0446\u0456\u0457 \u0434\u043B\u044F\u2026"
title: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\u043E\
  \u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438"
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
