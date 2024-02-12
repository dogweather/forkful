---
title:                "Видалення символів за візерунком"
date:                  2024-01-20T17:42:15.047774-07:00
model:                 gpt-4-1106-preview
simple_title:         "Видалення символів за візерунком"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Видалення символів за зразком — це процес викидання певних символів з рядка. Програмісти роблять це для очищення даних, валідації вводу, або форматування тексту під конкретні потреби.

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
