---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:49.317010-07:00
description: "\u041A\u0430\u043A: C# \u0434\u0435\u043B\u0430\u0435\u0442 \u043C\u0430\
  \u043D\u0438\u043F\u0443\u043B\u044F\u0446\u0438\u0438 \u0441 \u0442\u0435\u043A\
  \u0441\u0442\u043E\u043C \u0434\u043E\u0432\u043E\u043B\u044C\u043D\u043E \u043F\
  \u0440\u043E\u0441\u0442\u044B\u043C\u0438. \u041D\u0438\u0436\u0435, \u043E\u0431\
  \u0440\u0430\u0442\u0438\u0442\u0435 \u0432\u043D\u0438\u043C\u0430\u043D\u0438\u0435\
  \ \u043D\u0430 \u043C\u0435\u0442\u043E\u0434 `string.Replace` \u0434\u043B\u044F\
  \ \u0437\u0430\u043C\u0435\u043D\u044B \u0441\u043B\u043E\u0432."
lastmod: '2024-03-13T22:44:45.029007-06:00'
model: gpt-4-0125-preview
summary: "C# \u0434\u0435\u043B\u0430\u0435\u0442 \u043C\u0430\u043D\u0438\u043F\u0443\
  \u043B\u044F\u0446\u0438\u0438 \u0441 \u0442\u0435\u043A\u0441\u0442\u043E\u043C\
  \ \u0434\u043E\u0432\u043E\u043B\u044C\u043D\u043E \u043F\u0440\u043E\u0441\u0442\
  \u044B\u043C\u0438."
title: "\u041F\u043E\u0438\u0441\u043A \u0438 \u0437\u0430\u043C\u0435\u043D\u0430\
  \ \u0442\u0435\u043A\u0441\u0442\u0430"
weight: 10
---

## Как:
C# делает манипуляции с текстом довольно простыми. Ниже, обратите внимание на метод `string.Replace` для замены слов.

```C#
using System;

public class Program
{
    public static void Main()
    {
        string phrase = "Привет, Мир!";
        string updatedPhrase = phrase.Replace("Мир", "C#");
        
        Console.WriteLine(updatedPhrase); // Вывод: Привет, C#!
    }
}
```

Не ракетостроение, верно? Но что, если мы хотим игнорировать регистр или заменять только целые слова? Регулярные выражения на помощь:

```C#
using System;
using System.Text.RegularExpressions;

public class Program
{
    public static void Main()
    {
        string phrase = "Яблоки растут на деревьях. яблочные пироги вкусные.";
        string pattern = "\\bяблоко\\b"; // \b это граница слова в регулярных выражениях
        string replacement = "Апельсин";
        
        string updatedPhrase = Regex.Replace(phrase, pattern, replacement, RegexOptions.IgnoreCase);

        Console.WriteLine(updatedPhrase); // Вывод: Апельсины растут на деревьях. Апельсиновые пироги вкусные.
    }
}
```

## Глубокое Погружение
В прошлые времена манипулирование строками было сложным. У нас был только C, что означало работу с массивами символов и ручные итерации. C# подарил нам удобную работу со строками.

Если `string.Replace` или `Regex.Replace` не решают задачу, у нас есть варианты. Для больших текстов или сложных паттернов стоит подумать о написании собственного парсера или использовании библиотек, например, Antlr.

Регулярные выражения мощные для поиска паттернов, но могут быть медленными. Если производительность критична и вы увлекаетесь мелкими деталями, измеряйте и сравнивайте с `StringBuilder` для массовых, итеративных замен.

## Смотрите Также
- Документация Microsoft по [`string.Replace`](https://docs.microsoft.com/dotnet/api/system.string.replace)
- Класс [`Regex`](https://docs.microsoft.com/dotnet/api/system.text.regularexpressions.regex) в .NET для более сложных паттернов
- Ознакомьтесь с Antlr для сложного парсинга: [The ANTLR Mega Tutorial](https://tomassetti.me/antlr-mega-tutorial/)
