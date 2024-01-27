---
title:                "Using regular expressions"
date:                  2024-01-19
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions (regex) are patterns used to match string sequences. Programmers use them for searching, editing, or validating text. They're powerful and efficient, slicing through strings like a hot knife through butter.

## How to:
Let's look at matching, replacing, and splitting strings using regex in C#.

**Match a Phone Number:**

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

Output:
```
123-456-7890
987.654.3210
```

**Replace New Lines:**

```C#
using System;
using System.Text.RegularExpressions;

public class Example
{
    public static void Main()
    {
        string text = "First line.\nSecond line.\nThird line.";
        string pattern = @"\n";
        string replacement = " ";

        string result = Regex.Replace(text, pattern, replacement);
        Console.WriteLine(result);
    }
}
```

Output:
```
First line. Second line. Third line.
```

**Split a CSV:**

```C#
using System;
using System.Text.RegularExpressions;

public class Example
{
    public static void Main()
    {
        string text = "one,two,three,four";
        string pattern = @",";

        string[] substrings = Regex.Split(text, pattern);
        foreach (string match in substrings)
        {
            Console.WriteLine(match);
        }
    }
}
```

Output:
```
one
two
three
four
```

## Deep Dive
Regex has been around since the 1950s, thanks to mathematician Stephen Kleene. Alternatives to regex include string methods like `Contains`, `IndexOf`, `StartsWith`, etc., but they're less powerful for complex patterns.

Talking implementation, C#'s `Regex` class lives in `System.Text.RegularExpressions`. It leverages backtracking algorithms for pattern matching. Regex operations can be costly; use with care to avoid performance hits.

## See Also
- [Microsoft's Regex Documentation](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Regex Tester & Debugger](https://regex101.com/)
- [Mastering Regular Expressions](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/) by Jeffrey Friedl. _Note from [Robert](https://forkful.ai/en/about/): this is how I learned Regexes. I felt like I truly understood them after reading the book. And nowadays, I use the "Regex Tester & Debugger", listed above, when I have one to debug._
