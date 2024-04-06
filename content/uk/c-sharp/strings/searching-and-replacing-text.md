---
date: 2024-01-20 17:57:59.650156-07:00
description: "How to (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438) C# provides `String.Replace` method to replace text. Here's a simple example."
lastmod: '2024-04-05T22:38:48.322860-06:00'
model: gpt-4-1106-preview
summary: "How to (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  ) C# provides `String.Replace` method to replace text. Here's a simple example."
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
