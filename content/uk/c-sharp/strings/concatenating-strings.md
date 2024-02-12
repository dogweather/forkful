---
title:                "Об'єднання рядків"
aliases:
- /uk/c-sharp/concatenating-strings/
date:                  2024-01-20T17:34:29.528232-07:00
model:                 gpt-4-1106-preview
simple_title:         "Об'єднання рядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)

Concatenating strings is about sticking them together end-to-end. Programmers concatenate to build up texts dynamically, like creating messages or generating complex SQL queries.

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
