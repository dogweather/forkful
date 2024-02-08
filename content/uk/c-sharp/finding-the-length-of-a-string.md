---
title:                "Визначення довжини рядка"
aliases:
- uk/c-sharp/finding-the-length-of-a-string.md
date:                  2024-01-20T17:46:55.844354-07:00
model:                 gpt-4-1106-preview
simple_title:         "Визначення довжини рядка"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Finding the length of a string means determining how many characters it contains. Programmers often need this to validate input, loop through characters, or for string manipulation.

## How to: (Як робити:)
```C#
string example = "Привіт, світ!";
int length = example.Length;

Console.WriteLine(length); // Outputs: 13
```
```C#
string emptyString = String.Empty;
int emptyLength = emptyString.Length;

Console.WriteLine(emptyLength); // Outputs: 0
```

## Deep Dive (Занурення у глибину)
In C#, strings are immutable and represented by the `System.String` class. The `Length` property is integral to this class and returns the number of `Char` objects in the string, which is essentially the count of UTF-16 code units. Historically, as programming languages evolved, various methods to measure strings were developed, but in C#, `.Length` has been the go-to since its inception.

Other methods, like `StringInfo.LengthInTextElements`, can handle strings with combined characters (like emojis or accented characters) more accurately when counting user-perceived characters, as opposed to code units.

Under the hood, `Length` property is a simple and fast operation, retrieving a value pre-stored in the string object without actually iterating through it.

## See Also (Дивись також)
- [String.Length Property on Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.string.length)
- [Understanding Strings in C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- [StringInfo Class for Text Element Handling](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.stringinfo)
