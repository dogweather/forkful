---
date: 2024-01-20 17:42:15.047774-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) ."
lastmod: '2024-03-13T22:44:49.263442-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0412\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u0441\u0438\u043C\u0432\
  \u043E\u043B\u0456\u0432 \u0437\u0430 \u0432\u0456\u0437\u0435\u0440\u0443\u043D\
  \u043A\u043E\u043C"
weight: 5
---

## How to: (Як це зробити:)
```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string originalText = "Hello, Київ 2023!";
        string pattern = @"\d"; // Вираз для видалення цифр
        
        string cleanedText = Regex.Replace(originalText, pattern, "");
        
        Console.WriteLine(cleanedText); // Виведе "Hello, Київ !"
    }
}
```
Тут ми використовуємо `Regex.Replace`, щоб знайти всі цифри (`\d`) у рядку та замінити їх на порожній рядок, ефективно видаляючи їх.

## Deep Dive (Занурення у Деталі)
Історично, робота з текстами у програмуванні завжди була актуальною. Видалення символів за зразком — це одне з завдань, яке можливо здійснювати через регулярні вирази (regular expressions) ще з часів появи Perl. У C#, ця функціональність реалізована через клас `Regex` у просторі імен `System.Text.RegularExpressions`.

Альтернативи `Regex` включають методи `String.Replace()`, який замінює всі точні збіги з рядком, та Linq-операції для комплексних маніпуляцій з символами.

Використовуючи `Regex` ефективно вимагає розуміння синтаксису регулярних виразів, оскільки неефективні паттерни можуть сповільнити програму.

## See Also (Додатково)
- [`Regex` клас документація](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=netcore-3.1)
- [Регулярні вирази - довідник](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [LINQ в C# - огляд](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/)
