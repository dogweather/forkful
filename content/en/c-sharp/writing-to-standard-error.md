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

## Why

Writing to standard error, also known as stderr, is an important part of any C# programming project. It allows for better error handling and debugging, making it easier to identify and fix issues in your code.

## How To

Writing to standard error in C# is a simple process that can be accomplished using the `Console.Error` property. Here is an example of how to use it to write a custom error message:

```C#
Console.Error.WriteLine("Oops, something went wrong!");
```

The above code will write the message "Oops, something went wrong!" to the standard error stream. This can be helpful in situations where you want to differentiate between regular console output and error messages.

You can also use the `Console.SetError` method to redirect the standard error stream to a different location, such as a file. Here is an example of how to do this:

```C#
// Create a file to write errors to
FileStream file = new FileStream("errors.txt", FileMode.Create);
// Set standard error to write to the file
Console.SetError(new StreamWriter(file));

// Write an error message to the file
Console.Error.WriteLine("Oops, something went wrong!");

// Close the file stream
file.Close(); 
```

This can be useful if you want to save error messages for later analysis or logging.

## Deep Dive

Writing to standard error is not just limited to text messages. You can also write other types of data, such as objects or numbers. Here is an example of how to do this:

```C#
// Create a custom exception with a message and error code
Exception customException = new Exception("Custom error message", 404);

// Write the exception to standard error
Console.Error.WriteLine(customException.ToString());
```

The above code will write the exception object to standard error, including its message and error code. This can be helpful for more complex error handling scenarios.

It is important to note that any data written to standard error will not interrupt the execution of the program. Unlike standard output, which can be redirected and manipulated by another program, standard error is intended for error messages and will continue to be written to the console or designated output location.

## See Also

- [Official Microsoft documentation for Console.Error](https://docs.microsoft.com/en-us/dotnet/api/system.console.error?view=net-5.0)
- [C# error handling best practices](https://www.c-sharpcorner.com/blogs/error-handling-best-practices-with-c-sharp1)
- [Understanding the difference between standard output and standard error](https://www.techopedia.com/definition/25944/standard-output-stdout-and-standard-error-stderr)