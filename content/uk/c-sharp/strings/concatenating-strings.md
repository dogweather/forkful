---
date: 2024-01-20 17:34:29.528232-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) Here's how to concatenate strings in C#. Check out these examples."
lastmod: '2024-04-05T21:53:49.465635-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:)\
  \ Here's how to concatenate strings in C#."
title: "\u041E\u0431'\u0454\u0434\u043D\u0430\u043D\u043D\u044F \u0440\u044F\u0434\
  \u043A\u0456\u0432"
weight: 3
---

## How to: (Як це зробити:)
Here's how to concatenate strings in C#. Check out these examples:

```C#
string hello = "Привіт";
string world = "Світ";
string exclamation = "!";
string greeting = hello + ", " + world + exclamation;
Console.WriteLine(greeting); // Output: Привіт, Світ!
```

Or, use `StringBuilder` for better performance with lots of strings:

```C#
StringBuilder sb = new StringBuilder();
sb.Append("Привіт");
sb.Append(", ");
sb.Append("Світ");
sb.Append("!");
Console.WriteLine(sb.ToString()); // Output: Привіт, Світ!
```

## Deep Dive (Поглиблений Розбір):
Historically, C# developers used operators like `+` for simple concatenation. But this gets inefficient with numerous strings – every `+` creates a new string! That's where `StringBuilder` comes in; it's designed for heavy-duty string assembly, efficiently managing memory and processing.

Alternatives? Absolutely. You've got `String.Concat()`, `String.Format()`, or even string interpolation since C# 6:

```C#
string greeting = $"Привіт, {world}{exclamation}";
// Nice and clean.
```

Under the hood, concatenation with `+` or `$""` is compiled into `String.Concat()`. That's fine for a few strings, but with more, use `StringBuilder`.

## See Also (Дивіться також):
- Microsoft's official docs on string concatenation: [C# string concatenation](https://docs.microsoft.com/en-us/dotnet/csharp/how-to/concatenate-multiple-strings)
- Stack Overflow discussions about when to use `StringBuilder`: [Stack Overflow: StringBuilder vs. String Concat](https://stackoverflow.com/questions/73883/stringbuilder-vs-string-concatenation)
