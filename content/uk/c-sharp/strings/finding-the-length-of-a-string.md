---
date: 2024-01-20 17:46:55.844354-07:00
description: "How to: (\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438:) ."
lastmod: '2024-04-05T21:53:49.464767-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0412\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F \u0434\u043E\u0432\
  \u0436\u0438\u043D\u0438 \u0440\u044F\u0434\u043A\u0430"
weight: 7
---

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
