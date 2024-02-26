---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:00.998951-07:00
description: "\u0420\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u0435 \u0432\u044B\
  \u0440\u0430\u0436\u0435\u043D\u0438\u044F (regex) \u2013 \u044D\u0442\u043E \u0448\
  \u0430\u0431\u043B\u043E\u043D\u044B, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u0443\u0435\u043C\u044B\u0435 \u0434\u043B\u044F \u043F\u043E\u0438\u0441\u043A\
  \u0430 \u043F\u043E\u0441\u043B\u0435\u0434\u043E\u0432\u0430\u0442\u0435\u043B\u044C\
  \u043D\u043E\u0441\u0442\u0435\u0439 \u0432 \u0441\u0442\u0440\u043E\u043A\u0430\
  \u0445. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B\
  \ \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442 \u0438\u0445 \u0434\
  \u043B\u044F \u043F\u043E\u0438\u0441\u043A\u0430, \u0440\u0435\u0434\u0430\u043A\
  \u0442\u0438\u0440\u043E\u0432\u0430\u043D\u0438\u044F\u2026"
lastmod: '2024-02-25T18:49:42.692424-07:00'
model: gpt-4-0125-preview
summary: "\u0420\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u0435 \u0432\u044B\
  \u0440\u0430\u0436\u0435\u043D\u0438\u044F (regex) \u2013 \u044D\u0442\u043E \u0448\
  \u0430\u0431\u043B\u043E\u043D\u044B, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u0443\u0435\u043C\u044B\u0435 \u0434\u043B\u044F \u043F\u043E\u0438\u0441\u043A\
  \u0430 \u043F\u043E\u0441\u043B\u0435\u0434\u043E\u0432\u0430\u0442\u0435\u043B\u044C\
  \u043D\u043E\u0441\u0442\u0435\u0439 \u0432 \u0441\u0442\u0440\u043E\u043A\u0430\
  \u0445. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B\
  \ \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442 \u0438\u0445 \u0434\
  \u043B\u044F \u043F\u043E\u0438\u0441\u043A\u0430, \u0440\u0435\u0434\u0430\u043A\
  \u0442\u0438\u0440\u043E\u0432\u0430\u043D\u0438\u044F\u2026"
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u0445 \u0432\u044B\u0440\
  \u0430\u0436\u0435\u043D\u0438\u0439"
---

{{< edit_this_page >}}

## Что и почему?
Регулярные выражения (regex) – это шаблоны, используемые для поиска последовательностей в строках. Программисты используют их для поиска, редактирования или проверки текста. Они мощные и эффективные, проходя через строки, как горячий нож сквозь масло.

## Как использовать:
Давайте посмотрим на сопоставление, замену и разделение строк с использованием regex в C#.

**Сопоставление номера телефона:**

```C#
using System;
using System.Text.RegularExpressions;

public class Example
{
    public static void Main()
    {
        string pattern = @"\b\d{3}[-.]?\d{3}[-.]?\d{4}\b";
        string text = "Call me on 123-456-7890 or 987.654.3210.";
        MatchCollection matches = Regex.Matches(text, pattern);

        foreach (Match match in matches)
           Console.WriteLine(match.Value);
    }
}
```

Вывод:
```
123-456-7890
987.654.3210
```

**Замена переводов строк:**

```C#
using System;
using System.Text.RegularExpressions;

public class Example
{
    public static void Main()
    {
        string text = "Первая строка.\nВторая строка.\nТретья строка.";
        string pattern = @"\n";
        string replacement = " ";

        string result = Regex.Replace(text, pattern, replacement);
        Console.WriteLine(result);
    }
}
```

Вывод:
```
Первая строка. Вторая строка. Третья строка.
```

**Разделение CSV:**

```C#
using System;
using System.Text.RegularExpressions;

public class Example
{
    public static void Main()
    {
        string text = "один,два,три,четыре";
        string pattern = @",";

        string[] substrings = Regex.Split(text, pattern);
        foreach (string match in substrings)
        {
            Console.WriteLine(match);
        }
    }
}
```

Вывод:
```
один
два
три
четыре
```

## Погружение
Регулярные выражения существуют с 1950-х годов, благодаря математику Стивену Клини. Альтернативы regex включают строковые методы, такие как `Contains`, `IndexOf`, `StartsWith` и т.д., но они менее мощные для сложных шаблонов.

Говоря о реализации, класс `Regex` в C# находится в `System.Text.RegularExpressions`. Он использует алгоритмы обратного отслеживания для сопоставления шаблонов. Операции regex могут быть затратными; используйте с осторожностью, чтобы избежать потери производительности.

## Смотрите также
- [Документация по Regex от Microsoft](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Тестер и отладчик Regex](https://regex101.com/)
- [Освоение регулярных выражений](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/) от Джеффри Фридла. _Примечание от [Роберта](https://forkful.ai/en/about/): вот как я научился использовать Regex. Я по-настоящему их понял после прочтения этой книги. И в наши дни я использую "Тестер и отладчик Regex", указанный выше, когда мне нужно отладить что-то._
