---
title:                "Использование регулярных выражений"
aliases: - /ru/c-sharp/using-regular-expressions.md
date:                  2024-01-29T00:05:00.998951-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование регулярных выражений"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c-sharp/using-regular-expressions.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
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
