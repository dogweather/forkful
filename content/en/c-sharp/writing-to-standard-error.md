---
title:    "C# recipe: Writing to standard error"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Why
Writing to standard error is an essential aspect of C# programming that allows for efficient debugging and error handling. By sending error messages to the standard error stream, developers can easily identify and address any issues that may arise during the execution of their code.

## How To
To write to standard error in C#, developers can use the `Console.Error.WriteLine()` method. This method takes in a string as the parameter and prints it to the standard error stream. Here's a simple example:

```C#
// Writing an error message to standard error
Console.Error.WriteLine("An error has occurred!");
```

The above code will print the error message "An error has occurred!" to the standard error stream. Additionally, developers can also use `Console.Error.WriteLine()` in combination with the `try-catch` block to print out specific error messages for different types of exceptions:

```C#
try
{
    // Code that may cause an exception
}
catch (Exception ex)
{
    // Writing a custom error message to standard error
    Console.Error.WriteLine("An exception has occurred: " + ex.Message);
}
```

The above code will catch any type of exception that may occur within the `try` block and print a custom error message, along with the specific exception message, to the standard error stream.

## Deep Dive
When writing to standard error, it's important to note that the standard output and standard error streams are separate, which allows for a clear distinction between regular output and error messages. This can be useful when debugging or troubleshooting, as it allows developers to easily identify where an error has occurred.

It's also worth mentioning that the `Console.Error` object is a `TextWriter` class, which means developers can use other methods from this class, such as `Write()` and `WriteLine()`, to write to the standard error stream.

## See Also
- [Documentation for Console.Error.WriteLine()](https://docs.microsoft.com/en-us/dotnet/api/system.console.error.writeline?view=netcore-3.1)
- [Exception Handling in C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/exceptions/)

By utilizing the `Console.Error.WriteLine()` method, developers can easily write to the standard error stream and improve their code's error handling process. Make sure to also check out the documentation and other resources for a deeper understanding of this topic. Happy coding!