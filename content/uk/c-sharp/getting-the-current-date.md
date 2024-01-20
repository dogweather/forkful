---
title:                "Отримання поточної дати"
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що й Навіщо?
Отримання поточної дати в C# - це процес визначення дати і часу на момент виконання програми. Програмісти використовують це для відслідковування і зберігання інформації базового часу в програмах і системах.

## Як це зробити:
Отримати поточну дату в C# можна дуже легко завдяки класу `DateTime`. Нижче наведений код:
```C#
using System;
public class Program
{
    public static void Main(string[] args)
    {
        DateTime currentDate = DateTime.Now;
        Console.WriteLine(currentDate);
    }
}
```
При виконанні цього коду ви отримаєте поточну дату і час, наприклад:
```
2023-02-28 14:00:00
```

## Поглиблений аналіз
Метод `DateTime.Now` є частиною .Net Framework з його перших версій і є основним способом отримання поточної дати і часу в C#. 

Є інші альтернативи, наприклад, ви можете використовувати `DateTimeOffset.Now`, який надає інформацію про зміщення відносно UTC, що є корисним при роботі з різними часовими поясами. 

Метод `DateTime.Now` працює, отримуючи поточну дату і час системи, результат обчислюється на основі системного часу плюс властивість `DateTimeKind`, яка встановлена як `Local`.

## Дивитись також
- [DateTime в .NET](https://docs.microsoft.com/uk-ua/dotnet/api/system.datetime?view=net-6.0)
- [DateTime vs DateTimeOffset](https://docs.microsoft.com/uk-ua/dotnet/standard/datetime/choosing-between-datetime)