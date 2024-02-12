---
title:                "Використання регулярних виразів"
aliases:
- /uk/c-sharp/using-regular-expressions/
date:                  2024-02-03T19:16:59.502107-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання регулярних виразів"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Регулярні вирази (regex) у C# є потужним інструментом для пошуку збігів у рядках, що дозволяє програмістам ефективно здійснювати пошук, заміну, розбиття або витягування даних. Програмісти використовують regex для завдань, які варіюються від простої валідації, як перевірка формату електронної пошти, до складних завдань обробки тексту через їхню гнучкість та продуктивність.

## Як:

### Простий пошук збігів
Щоб перевірити, чи містить рядок конкретний шаблон, ви можете використати метод `Regex.IsMatch` з простору імен `System.Text.RegularExpressions`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Привіт, Світ!";
        string pattern = "Світ";
        bool containsPattern = Regex.IsMatch(sampleText, pattern);

        Console.WriteLine(containsPattern);  // Вивід: True
    }
}
```

### Витягування даних
Для витягування даних з рядка за допомогою груп у regex можна скористатися методом `Regex.Match`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Дата: 2023-04-12";
        string pattern = @"Дата: (\d{4})-(\d{2})-(\d{2})";
        Match match = Regex.Match(sampleText, pattern);

        if (match.Success)
        {
            Console.WriteLine($"Рік: {match.Groups[1].Value}");  // Вивід: Рік: 2023
            Console.WriteLine($"Місяць: {match.Groups[2].Value}");  // Вивід: Місяць: 04
            Console.WriteLine($"День: {match.Groups[3].Value}");  // Вивід: День: 12
        }
    }
}
```

### Заміна тексту
Метод `Regex.Replace` дозволяє замінювати текст у рядку, який відповідає вказаному шаблону.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Відвідайте Microsoft!";
        string pattern = "Microsoft";
        string replacement = "Google";

        string result = Regex.Replace(sampleText, pattern, replacement);

        Console.WriteLine(result);  // Вивід: Відвідайте Google!
    }
}
```

### Розбиття рядків
Ви можете розділити рядок на масив на основі шаблону regex, використовуючи метод `Regex.Split`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "один,два,три,чотири,п'ять";
        string pattern = ",";

        string[] result = Regex.Split(sampleText, pattern);

        foreach (string item in result)
        {
            Console.WriteLine(item);
        }
        // Вивід: 
        // один
        // два
        // три
        // чотири
        // п'ять
    }
}
```

### Використання сторонніх бібліотек
Попри те що .NET Framework надає широкі можливості для регулярних виразів, існують також сторонні бібліотеки, як-от `PCRE.NET`, що пропонують регулярні вирази, сумісні з Perl (PCRE) у C#. Це може бути корисно, якщо вам потрібні можливості або синтаксис мотора регулярних виразів Perl, які не доступні у реалізації .NET.

Щоб використовувати `PCRE.NET`, вам спочатку потрібно встановити його пакет NuGet, а потім можете використовувати його подібно до того, як ви використовуєте класи регулярних виразів рідного .NET.

```csharp
// Приклад використання PCRE.NET тут
// Примітка: Уявіть зразок, схожий на вищезазначені, адаптований для демонстрації унікальної можливості PCRE.NET.
```

При інтеграції сторонніх бібліотек для регулярних виразів завжди консультуйтеся з їхньою документацією для отримання детальної інформації про використання та сумісність.
