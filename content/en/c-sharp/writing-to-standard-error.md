---
title:    "C# recipe: Writing to standard error"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

As a programmer, you may have encountered errors while running your code. These errors can be helpful in troubleshooting and debugging, but they can also be quite frustrating. That's where standard error comes in. Writing to standard error allows you to specifically display error messages, making it easier to identify and address any issues in your code.

## How To

To write to standard error in C#, you will need to use the `Console` class and its `Error` property. This property allows you to write to the standard error stream rather than the standard output stream. Let's take a look at some code examples:

```
using System;

// Direct output to standard error
Console.Error.WriteLine("Oops, something went wrong!");
```

In this first example, we use the `Error` property to write a string to the standard error stream. This will display the string on the console with a red font, indicating that it is an error message.

```
using System;

// Demonstrating a divide by zero error 
int num = 10;
int div = 0;

// Direct output to standard error
Console.Error.WriteLine("Attempting division...");
Console.Error.WriteLine("Result: " + (num / div));
```

In this next example, we intentionally divide by zero to generate an error. By using `Console.Error`, we can output a message before the error occurs, helping us to better understand the context of the error.

The output for both of these examples would look like this in a console:

```
Oops, something went wrong!
Attempting division...
Unhandled exception. System.DivideByZeroException: Attempted to divide by zero.
   at Program.Main()
```

As you can see, using `Console.Error.WriteLine()` allows us to write to the standard error stream and provide additional information about the error.

## Deep Dive

When writing to standard error, it is important to consider the difference between the standard output and standard error streams. The standard output stream is meant for regular program output, while the standard error stream is specifically for error messages.

Additionally, it is worth noting that you can redirect the standard error stream to a file rather than displaying it on the console. This can be helpful in situations where you need to save and analyze error messages.

## See Also

- [C# Console Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.console?view=netcore-3.1)
- [C# Error Messages Tutorial](https://www.tutorialspoint.com/cplusplus/cpp_error_handling.htm)
- [Redirecting Standard Error Stream in C#](https://www.dotnetperls.com/redirectstandarderror)

Writing to standard error may seem like a small and simple concept, but it can greatly improve your debugging process. By using the `Console.Error` property, you can easily differentiate between regular program output and error messages, making troubleshooting and fixing errors much easier. Happy coding!