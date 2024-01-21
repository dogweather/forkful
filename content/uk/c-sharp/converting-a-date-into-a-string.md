---
title:                "Перетворення дати в рядок"
date:                  2024-01-20T17:36:43.369223-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення дати в рядок"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

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