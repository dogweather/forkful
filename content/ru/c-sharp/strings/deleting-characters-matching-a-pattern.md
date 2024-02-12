---
title:                "Удаление символов, соответствующих шаблону"
aliases: - /ru/c-sharp/deleting-characters-matching-a-pattern.md
date:                  2024-01-28T23:57:32.016445-07:00
model:                 gpt-4-0125-preview
simple_title:         "Удаление символов, соответствующих шаблону"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c-sharp/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Удаление символов, соответствующих шаблону, заключается в поиске и удалении определенных последовательностей символов из строк на основе правил (например, регулярных выражений). Программисты делают это для очистки данных, валидации ввода или манипуляции текстом для различных целей.

## Как:
Хотите избавиться от некоторых символов? Вот как это сделать на C#:

```C#
using System;
using System.Text.RegularExpressions;

class PatternDeletion
{
    static void Main()
    {
        string originalText = "B4n4n4 P1zza!";
        string pattern = @"[0-9]+"; // Удаляем все цифры
        
        string cleanedText = Regex.Replace(originalText, pattern, string.Empty);
        
        Console.WriteLine(cleanedText); // Вывод: Bnnn Pzza!
    }
}
```
Нужно обрезать 'a' за которым следует цифра? Взгляните:

```C#
string targetedRemoval = "C4ndy C4ne";
string complexPattern = @"a[0-9]"; // Нацелено на 'a', за которым следует любая цифра

string refinedText = Regex.Replace(targetedRemoval, complexPattern, string.Empty);

Console.WriteLine(refinedText); // Вывод: Cndy Cne
```

## Глубже копаем
Regex (Регулярные Выражения) стоят за способностью сопоставлять шаблоны, возвращаясь к теоретическим корням в 1950-х годах (спасибо, теория автоматов!). Альтернативы регулярным выражениям включают простой `String.Replace()` для более простых замен или настраиваемые алгоритмы, если критична производительность (поскольку регулярные выражения имеют некоторый излишек). Эти альтернативы не обладают гибкостью и точностью, которые делают регулярные выражения идеальным инструментом для работы со сложными шаблонами. Реализуя удаление по шаблону, будьте внимательны к двуострому мечу регулярных выражений – они мощные, но могут быть криптическими и медленными для обработки обширных данных.

## Смотрите также
- Документация по Regex от Microsoft: https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions
- Regex101 (для тестирования шаблонов регулярных выражений): https://regex101.com/
- Введение в теорию автоматов: https://en.wikipedia.org/wiki/Automata_theory
