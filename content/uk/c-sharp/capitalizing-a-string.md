---
title:                "Робимо першу літеру рядка великою"
html_title:           "C#: Робимо першу літеру рядка великою"
simple_title:         "Робимо першу літеру рядка великою"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що і навіщо?

"Капіталізація рядка" - це процес перетворення перших букв усіх слів рядка в верхній реєстр. Програмісти роблять це, щоб використовувати текст у відповідних контекстах, наприклад, для заголовків або при виведенні даних.

## Як це зробити:

Для капіталізації рядка в C# ми можемо використати метод TextInfo.ToTitleCase(). Давайте дивитися на програму-приклад:

```C#
using System;
using System.Globalization;

class CapitalizeString
{
    static void Main()
    {
        string text = "привіт, світе!";
        string capitalized = CultureInfo.CurrentCulture.TextInfo.ToTitleCase(text);
        Console.WriteLine(capitalized);
    }
}
```
При виконанні цього коду вивід буде наступний:

```C#
"Привіт, Світе!"
```
## Поглиблено:

1. **Історичний контекст**: Строки в C# були завжди незмінними, тому всі оператори та методи виконуються не через зміну існуючого екземпляра, а через створення нового. Метод ToTitleCase() є частиною namespaces в .NET з його створення.

2. **Альтернативи**: Іншим способом капіталізації рядка може бути використання LINQ. Це корисно, якщо метод CultureInfo.CurrentCulture.TextInfo.ToTitleCase() недоступний або необхідний функціонал, якого в ньому немає.

3. **Деталі реалізації**: Метод ToTitleCase() не змінює розмір існуючих символів у рядку. Він тільки перетворює на верхній реєстр перший символ кожного слова, якщо цей символ написаний маленькою буквою.

## Більше інформації:

- Про роботу з рядками в C#: [Microsoft C# Guide](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- Про метод TextInfo.ToTitleCase(): [Microsoft Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase?view=net-5.0)
- Про маніпуляції з рядками за допомогою LINQ: [Dot Net Perls](https://www.dotnetperls.com/string-linq)