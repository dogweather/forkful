---
date: 2024-01-20 17:36:43.369223-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) \u041A\u043E\u043D\u0432\u0435\u0440\u0442\u0430\u0446\u0456\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A \u0456\u0441\u043D\u0443\
  \u0454 \u0432\u0456\u0434 \u0447\u0430\u0441\u0456\u0432 \u0440\u0430\u043D\u043D\
  \u0456\u0445 \u043C\u043E\u0432 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\
  \u0432\u0430\u043D\u043D\u044F, \u0430\u0434\u0436\u0435 \u043F\u043E\u0442\u0440\
  \u0435\u0431\u0430 \u0432\u0456\u0434\u043E\u0431\u0440\u0430\u0436\u0435\u043D\u043D\
  \u044F \u0434\u0430\u0442 \u0434\u043B\u044F \u043B\u044E\u0434\u0435\u0439 \u0437\
  \u0430\u0432\u0436\u0434\u0438 \u0431\u0443\u043B\u0430\u2026"
lastmod: '2024-04-05T22:51:02.389892-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:)\
  \ \u041A\u043E\u043D\u0432\u0435\u0440\u0442\u0430\u0446\u0456\u044F \u0434\u0430\
  \u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A \u0456\u0441\u043D\u0443\u0454\
  \ \u0432\u0456\u0434 \u0447\u0430\u0441\u0456\u0432 \u0440\u0430\u043D\u043D\u0456\
  \u0445 \u043C\u043E\u0432 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\u0432\
  \u0430\u043D\u043D\u044F, \u0430\u0434\u0436\u0435 \u043F\u043E\u0442\u0440\u0435\
  \u0431\u0430 \u0432\u0456\u0434\u043E\u0431\u0440\u0430\u0436\u0435\u043D\u043D\u044F\
  \ \u0434\u0430\u0442 \u0434\u043B\u044F \u043B\u044E\u0434\u0435\u0439 \u0437\u0430\
  \u0432\u0436\u0434\u0438 \u0431\u0443\u043B\u0430 \u0430\u043A\u0442\u0443\u0430\
  \u043B\u044C\u043D\u043E\u044E."
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A"
weight: 28
---

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
