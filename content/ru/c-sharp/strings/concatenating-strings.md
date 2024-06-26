---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:53.508855-07:00
description: "\u041A\u0430\u043A: \u041A\u043E\u043D\u043A\u0430\u0442\u0435\u043D\
  \u0430\u0446\u0438\u044F \u0441\u0442\u0440\u043E\u043A \u0432 C# \u043C\u043E\u0436\
  \u0435\u0442 \u0431\u044B\u0442\u044C \u0432\u044B\u043F\u043E\u043B\u043D\u0435\
  \u043D\u0430 \u043D\u0435\u0441\u043A\u043E\u043B\u044C\u043A\u0438\u043C\u0438\
  \ \u0441\u043F\u043E\u0441\u043E\u0431\u0430\u043C\u0438: \u0418\u0441\u043F\u043E\
  \u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435 \u043E\u043F\u0435\u0440\u0430\
  \u0442\u043E\u0440\u0430 `+`."
lastmod: '2024-03-13T22:44:45.041565-06:00'
model: gpt-4-0125-preview
summary: "\u041A\u043E\u043D\u043A\u0430\u0442\u0435\u043D\u0430\u0446\u0438\u044F\
  \ \u0441\u0442\u0440\u043E\u043A \u0432 C# \u043C\u043E\u0436\u0435\u0442 \u0431\
  \u044B\u0442\u044C \u0432\u044B\u043F\u043E\u043B\u043D\u0435\u043D\u0430 \u043D\
  \u0435\u0441\u043A\u043E\u043B\u044C\u043A\u0438\u043C\u0438 \u0441\u043F\u043E\u0441\
  \u043E\u0431\u0430\u043C\u0438."
title: "\u0421\u043A\u043B\u0435\u0438\u0432\u0430\u043D\u0438\u0435 \u0441\u0442\u0440\
  \u043E\u043A"
weight: 3
---

## Как:
Конкатенация строк в C# может быть выполнена несколькими способами:

Использование оператора `+`: 
```C#
string hello = "Привет";
string world = "Мир";
string concatenated = hello + ", " + world + "!";
Console.WriteLine(concatenated); // Вывод: Привет, Мир!
```

Использование метода `String.Concat()`:
```C#
string concatenated = String.Concat("Привет", ", ", "Мир", "!");
Console.WriteLine(concatenated); // Вывод: Привет, Мир!
```

Использование `StringBuilder` для эффективности в циклах:
```C#
StringBuilder sb = new StringBuilder();
sb.Append("Привет");
sb.Append(", ");
sb.Append("Мир");
sb.Append("!");
Console.WriteLine(sb.ToString()); // Вывод: Привет, Мир!
```

Использование интерполяции строк (C# 6.0 и выше):
```C#
string world = "Мир";
string concatenated = $"Привет, {world}!";
Console.WriteLine(concatenated); // Вывод: Привет, Мир!
```

## Подробнее
Конкатенация строк не новость; она существует с первых дней программирования. Однако, способы ее выполнения в C# эволюционировали. Изначально широко использовался оператор `+`, но он не всегда эффективен, особенно в циклах, потому что строки в .NET неизменяемы. Каждая операция `+` создает новую строку, что может привести к проблемам производительности.

`String.Concat()` - это прямой вызов метода, который также не подходит для циклов, но подходит для объединения небольшого, известного количества строк.

`StringBuilder` - это предпочтительный выбор для сценариев с циклами или при постепенном построении строки. Под капотом `StringBuilder` поддерживает буфер для добавления без создания новых строк для каждой операции добавления.

Интерполяция строк, введенная в C# 6.0, позволяет коду быть более читабельным и поддерживаемым. На этапе компиляции она превращается в вызов `String.Format()`, но выглядит проще и менее подвержена ошибкам.

Каждый метод имеет свое место: быстрые конкатенации (`+`), объединение нескольких строк (`String.Concat()`), интенсивное построение строк (`StringBuilder`) и чистые, форматированные строки (интерполяция строк).

## Смотрите также
- Microsoft Docs по конкатенации строк: [Конкатенация строк](https://docs.microsoft.com/en-us/dotnet/csharp/how-to/concatenate-multiple-strings)
- Microsoft Docs о `StringBuilder`: [Класс StringBuilder](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder)
