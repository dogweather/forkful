---
date: 2024-01-20 17:36:43.369223-07:00
description: "\u041A\u043E\u043D\u0432\u0435\u0440\u0442\u0430\u0446\u0456\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A \u0434\u043E\u0437\u0432\
  \u043E\u043B\u044F\u0454 \u043F\u0440\u0435\u0434\u0441\u0442\u0430\u0432\u0438\u0442\
  \u0438 \u0434\u0430\u0442\u0443 \u0432 \u0437\u0440\u0443\u0447\u043D\u043E\u043C\
  \u0443 \u0434\u043B\u044F \u043B\u044E\u0434\u0438\u043D\u0438 \u0444\u043E\u0440\
  \u043C\u0430\u0442\u0456. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\
  \u0442\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\
  \u0442\u044C \u0446\u0435, \u0449\u043E\u0431 \u0437\u0431\u0435\u0440\u0435\u0433\
  \u0442\u0438 \u0434\u0430\u0442\u0438 \u0432 \u0431\u0430\u0437\u0430\u0445 \u0434\
  \u0430\u043D\u0438\u0445, \u0432\u0438\u0432\u0435\u0441\u0442\u0438\u2026"
lastmod: '2024-03-13T22:44:49.308604-06:00'
model: gpt-4-1106-preview
summary: "\u041A\u043E\u043D\u0432\u0435\u0440\u0442\u0430\u0446\u0456\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A \u0434\u043E\u0437\u0432\
  \u043E\u043B\u044F\u0454 \u043F\u0440\u0435\u0434\u0441\u0442\u0430\u0432\u0438\u0442\
  \u0438 \u0434\u0430\u0442\u0443 \u0432 \u0437\u0440\u0443\u0447\u043D\u043E\u043C\
  \u0443 \u0434\u043B\u044F \u043B\u044E\u0434\u0438\u043D\u0438 \u0444\u043E\u0440\
  \u043C\u0430\u0442\u0456. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\
  \u0442\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\
  \u0442\u044C \u0446\u0435, \u0449\u043E\u0431 \u0437\u0431\u0435\u0440\u0435\u0433\
  \u0442\u0438 \u0434\u0430\u0442\u0438 \u0432 \u0431\u0430\u0437\u0430\u0445 \u0434\
  \u0430\u043D\u0438\u0445, \u0432\u0438\u0432\u0435\u0441\u0442\u0438\u2026"
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A"
weight: 28
---

## What & Why? (Що та Чому?)
Конвертація дати в рядок дозволяє представити дату в зручному для людини форматі. Програмісти використовують це, щоб зберегти дати в базах даних, вивести їх на екран, або здійснити локалізацію форматів дат.

## How to: (Як це зробити:)
```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        DateTime now = DateTime.Now;
        
        // Convert to a simple date string
        string simpleDateString = now.ToString("d");
        Console.WriteLine(simpleDateString); // Output example: 3/16/2023
        
        // Convert using custom formats
        string customFormatted = now.ToString("dd-MM-yyyy HH:mm");
        Console.WriteLine(customFormatted); // Output example: 16-03-2023 12:34
        
        // Convert with culture info (Ukrainian)
        CultureInfo cultureInfo = new CultureInfo("uk-UA");
        string localizedDate = now.ToString(cultureInfo);
        Console.WriteLine(localizedDate); // Output example: четвер, 16 березня 2023 р. 12:34:56
    }
}

```

## Deep Dive (Поглиблений Аналіз)
Конвертація дати в рядок існує від часів ранніх мов програмування, адже потреба відображення дат для людей завжди була актуальною. В C#, System.DateTime і метод ToString() дають велику гнучкість для цього завдання, дозволяючи використовувати стандартні і користувацькі формати, а також локалізовані шаблони з CultureInfo. Альтернативами є інші класи, наприклад DateTimeOffset або бібліотека NodaTime для складніших завдань, пов'язаних з датами.

## See Also (Дивіться також):
- [Microsoft Docs: Standard Date and Time Format Strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings)
- [Microsoft Docs: Custom Date and Time Format Strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [Microsoft Docs: CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)
