---
title:                "Пошук та заміна тексту"
date:                  2024-01-20T17:57:59.650156-07:00
model:                 gpt-4-1106-preview
simple_title:         "Пошук та заміна тексту"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Searching and replacing text — це процес знаходження певного фрагмента тексту та його заміни на інший. Програмісти використовують це для автоматизації редагування коду, швидкого виправлення помилок, або заміни змінних у шаблонах.

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
