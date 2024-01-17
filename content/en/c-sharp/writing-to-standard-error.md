---
title:                "Writing to standard error"
html_title:           "C# recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error in C# refers to the act of redirecting error messages to a separate channel known as the "Standard Error Stream". This is typically done in situations where traditional error handling methods, like exceptions, may not be sufficient. Programmers use this method to ensure that critical errors are recorded and communicated to the user, even if the program crashes.

## How to:

To write to standard error in C#, you can use the `Console.Error.WriteLine()` method. This takes a string as input and outputs it to the standard error stream. Here's an example:

```C#
Console.Error.WriteLine("Error: Invalid input");
```

This will print the error message to the standard error stream, which can then be captured and displayed to the user. 

## Deep Dive:

In the early days of programming, error messages were simply written to the console along with regular output. However, this made it difficult for users to distinguish between standard output and errors, especially in long programs. To address this issue, the concept of standard error was introduced, providing a separate channel for error messages. This also allows for better logging and debugging of errors.

There are other alternatives to writing to standard error, such as using exceptions or creating custom error handling methods. However, these may not always be feasible or appropriate. Writing to standard error allows for a more straightforward and immediate way of communicating critical errors to users.

In terms of implementation, standard error is typically handled by the operating system. In C#, the common language runtime (CLR) handles the standard error stream, making it easily accessible for programmers. Additionally, the standard error stream is usually redirected to the console by default. However, this can be customized as needed.

## See Also:

- [Microsoft Docs: Console.Error Property](https://docs.microsoft.com/en-us/dotnet/api/system.console.error)
- [Codeguru: Writing to the Standard Error Stream in .NET C#](https://www.codeguru.com/csharp/writing-to-the-standard-error-stream-in-.net-c.html)
- [C# Corner: Standard Error and Standard Output in C#](https://www.c-sharpcorner.com/article/standard-error-and-standard-output-in-c-sharp/)