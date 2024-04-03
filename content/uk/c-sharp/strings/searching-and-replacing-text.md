---
date: 2024-01-20 17:57:59.650156-07:00
description: "Searching and replacing text \u2014 \u0446\u0435 \u043F\u0440\u043E\u0446\
  \u0435\u0441 \u0437\u043D\u0430\u0445\u043E\u0434\u0436\u0435\u043D\u043D\u044F\
  \ \u043F\u0435\u0432\u043D\u043E\u0433\u043E \u0444\u0440\u0430\u0433\u043C\u0435\
  \u043D\u0442\u0430 \u0442\u0435\u043A\u0441\u0442\u0443 \u0442\u0430 \u0439\u043E\
  \u0433\u043E \u0437\u0430\u043C\u0456\u043D\u0438 \u043D\u0430 \u0456\u043D\u0448\
  \u0438\u0439. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438\
  \ \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0442\
  \u044C \u0446\u0435 \u0434\u043B\u044F \u0430\u0432\u0442\u043E\u043C\u0430\u0442\
  \u0438\u0437\u0430\u0446\u0456\u0457\u2026"
lastmod: '2024-03-13T22:44:49.264997-06:00'
model: gpt-4-1106-preview
summary: "Searching and replacing text \u2014 \u0446\u0435 \u043F\u0440\u043E\u0446\
  \u0435\u0441 \u0437\u043D\u0430\u0445\u043E\u0434\u0436\u0435\u043D\u043D\u044F\
  \ \u043F\u0435\u0432\u043D\u043E\u0433\u043E \u0444\u0440\u0430\u0433\u043C\u0435\
  \u043D\u0442\u0430 \u0442\u0435\u043A\u0441\u0442\u0443 \u0442\u0430 \u0439\u043E\
  \u0433\u043E \u0437\u0430\u043C\u0456\u043D\u0438 \u043D\u0430 \u0456\u043D\u0448\
  \u0438\u0439."
title: "\u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\
  \u0430 \u0442\u0435\u043A\u0441\u0442\u0443"
weight: 10
---

## How to (Як це зробити)
C# provides `String.Replace` method to replace text. Here's a simple example:

```C#
using System;

public class TextReplaceExample
{
    static void Main()
    {
        string originalText = "Привіт, як справи?";
        string newText = originalText.Replace("справи", "життя");

        Console.WriteLine(newText);
    }
}
```

Output:
```
Привіт, як життя?
```

For more complex patterns, you can use `Regex.Replace` from the `System.Text.RegularExpressions` namespace:

```C#
using System;
using System.Text.RegularExpressions;

class RegexReplaceExample
{
    static void Main()
    {
        string input = "Купи 10 апельсинів і 12 бананів.";
        string pattern = @"\d+";
        string replacement = "#";

        string result = Regex.Replace(input, pattern, replacement);
        Console.WriteLine(result);
    }
}
```

Output:
```
Купи # апельсинів і # бананів.
```

## Deep Dive (Поглиблений Огляд)
Searching and replacing text has been around since the early days of computing. Editors like `sed` in Unix made it popular in the '70s. In .NET, `String.Replace` is straightforward for simple text changes, but `Regex` is powerful for patterns.

Alternatives include text-processing tools or integrated development environment (IDE) features like Visual Studio's "Find and Replace".

Implementation-wise, beware of memory overhead with large strings when using `String.Replace`. `StringBuilder` can be more efficient. `Regex` has a performance cost but is optimized for complex patterns.

## See Also (Дивіться Також)
- Microsoft Docs on `String.Replace`: [https://docs.microsoft.com/en-us/dotnet/api/system.string.replace](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace)
- Microsoft Docs on `Regex.Replace`: [https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace)
- An intro to `StringBuilder` for C#: [https://docs.microsoft.com/en-us/dotnet/standard/base-types/stringbuilder](https://docs.microsoft.com/en-us/dotnet/standard/base-types/stringbuilder)
- `sed` — an introduction to the stream editor: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
