---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:32.016445-07:00
description: "\u041A\u0430\u043A: \u0425\u043E\u0442\u0438\u0442\u0435 \u0438\u0437\
  \u0431\u0430\u0432\u0438\u0442\u044C\u0441\u044F \u043E\u0442 \u043D\u0435\u043A\
  \u043E\u0442\u043E\u0440\u044B\u0445 \u0441\u0438\u043C\u0432\u043E\u043B\u043E\u0432\
  ? \u0412\u043E\u0442 \u043A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\
  \u0430\u0442\u044C \u043D\u0430 C#."
lastmod: '2024-04-05T22:40:31.805599-06:00'
model: gpt-4-0125-preview
summary: "\u0425\u043E\u0442\u0438\u0442\u0435 \u0438\u0437\u0431\u0430\u0432\u0438\
  \u0442\u044C\u0441\u044F \u043E\u0442 \u043D\u0435\u043A\u043E\u0442\u043E\u0440\
  \u044B\u0445 \u0441\u0438\u043C\u0432\u043E\u043B\u043E\u0432?"
title: "\u0423\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u0441\u0438\u043C\u0432\u043E\
  \u043B\u043E\u0432, \u0441\u043E\u043E\u0442\u0432\u0435\u0442\u0441\u0442\u0432\
  \u0443\u044E\u0449\u0438\u0445 \u0448\u0430\u0431\u043B\u043E\u043D\u0443"
weight: 5
---

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
