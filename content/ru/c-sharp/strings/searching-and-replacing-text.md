---
title:                "Поиск и замена текста"
aliases:
- /ru/c-sharp/searching-and-replacing-text/
date:                  2024-01-29T00:01:49.317010-07:00
model:                 gpt-4-0125-preview
simple_title:         "Поиск и замена текста"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c-sharp/searching-and-replacing-text.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Поиск и замена текста в строках позволяют обновлять данные без ручных правок. Программистам это необходимо для корректировки пользовательского ввода, форматирования данных или пакетных обновлений эффективным способом.

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
