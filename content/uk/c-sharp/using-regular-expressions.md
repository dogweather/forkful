---
title:                "Використання регулярних виразів"
date:                  2024-01-19
html_title:           "Bash: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Використання регулярних виразів дає змогу фільтрувати та маніпулювати текстом за допомогою паттернів. Програмісти користуються ними, щоб швидко обробляти великі об'єми текстів, валідувати дані чи витягувати інформацію.

## Як це зробити:
```C#
using System;
using System.Text.RegularExpressions;

class RegexExample
{
    static void Main()
    {
        string pattern = @"\b[A-Za-z]{6,}\b";
        string text = "Regex is powerful and often quite useful in programming.";

        MatchCollection matches = Regex.Matches(text, pattern);

        Console.WriteLine("Words with 6 or more letters:");
        foreach (Match match in matches)
        {
            Console.WriteLine(match.Value);
        }
    }
}
```
Виведення:
```
powerful
programming
```

## Поглиблений аналіз:
Створені у 1950-х, регулярні вирази наснажили UNIX utilities. Зараз є інші інструменти як LINQ чи парсери для складних задач, та регулярні вирази — оптимальний вибір для текстових паттернів. С# використовує клас `Regex` з `System.Text.RegularExpressions`.

## Додаткове джерело:
- Microsoft документація про `Regex`: https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex
- Про регулярні вирази загалом: https://www.regular-expressions.info/
- Онлайн-інструменти для тестування регулярних виразів, як Regex101: https://regex101.com/
