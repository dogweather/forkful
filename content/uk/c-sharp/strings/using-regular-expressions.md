---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:59.502107-07:00
description: "\u042F\u043A: \u0429\u043E\u0431 \u043F\u0435\u0440\u0435\u0432\u0456\
  \u0440\u0438\u0442\u0438, \u0447\u0438 \u043C\u0456\u0441\u0442\u0438\u0442\u044C\
  \ \u0440\u044F\u0434\u043E\u043A \u043A\u043E\u043D\u043A\u0440\u0435\u0442\u043D\
  \u0438\u0439 \u0448\u0430\u0431\u043B\u043E\u043D, \u0432\u0438 \u043C\u043E\u0436\
  \u0435\u0442\u0435 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u0442\u0438\
  \ \u043C\u0435\u0442\u043E\u0434 `Regex.IsMatch` \u0437 \u043F\u0440\u043E\u0441\
  \u0442\u043E\u0440\u0443 \u0456\u043C\u0435\u043D `System.Text.RegularExpressions`."
lastmod: '2024-03-13T22:44:49.273450-06:00'
model: gpt-4-0125-preview
summary: "\u0429\u043E\u0431 \u043F\u0435\u0440\u0435\u0432\u0456\u0440\u0438\u0442\
  \u0438, \u0447\u0438 \u043C\u0456\u0441\u0442\u0438\u0442\u044C \u0440\u044F\u0434\
  \u043E\u043A \u043A\u043E\u043D\u043A\u0440\u0435\u0442\u043D\u0438\u0439 \u0448\
  \u0430\u0431\u043B\u043E\u043D, \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435\
  \ \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u0442\u0438 \u043C\u0435\
  \u0442\u043E\u0434 `Regex.IsMatch` \u0437 \u043F\u0440\u043E\u0441\u0442\u043E\u0440\
  \u0443 \u0456\u043C\u0435\u043D `System.Text.RegularExpressions`."
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0440\
  \u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0438\u0445 \u0432\u0438\u0440\u0430\u0437\
  \u0456\u0432"
weight: 11
---

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
