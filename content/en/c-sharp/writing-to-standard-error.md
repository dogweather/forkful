---
title:                "Writing to standard error"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error (stderr) means sending your error messages separate from the regular output (stdout). Programmers do this to separate normal data from error info, which helps in logging and debugging.

## How to:

In C#, write to stderr using `Console.Error.WriteLine()`. It’s similar to `Console.WriteLine()`, just aimed at the error stream.

```C#
using System;

class Program
{
    static void Main()
    {
        Console.WriteLine("Standard Output Message."); // Goes to stdout
        Console.Error.WriteLine("Error Message!"); // Goes to stderr
    }
}
```

Sample output when all is well:

```
Standard Output Message.
```

But, if something's off, you'd see:

```
Standard Output Message.
Error Message!
```

The error message pops up in the console or can be redirected to a file.

## Deep Dive

Historically, separating stdout and stderr dates back to Unix systems where it allowed clean data processing and error handling. In C# (and .NET in general), `Console.Out` represents stdout, while `Console.Error` represents stderr.

You can redirect both using `Console.SetOut()` and `Console.SetError()`. Streams like `FileStream` or `StringWriter` can snag the output for logging. It's crucial in scenarios where error messages shouldn't mix with regular data, say, when stdout is piped to another program.

## See Also

- [Console.Error Property - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.console.error)
- [.NET Stream Class - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.stream)
