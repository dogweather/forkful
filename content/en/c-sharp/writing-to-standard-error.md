---
title:                "C# recipe: Writing to standard error"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why
Writing to the standard error in C# may seem like a small aspect of programming, but it can be incredibly useful in certain situations. Standard error, also known as stderr, is a stream used to display error messages from a program. By utilizing this feature, you can easily differentiate between normal output and error messages, making it easier to debug code and troubleshoot issues.

## How To
To write to standard error in C#, we first need to understand the Stream class. Streams are used to read or write data to different sources, such as files or network connections. In this case, we will be using the standard error stream, which is represented by the "Console.Error" property.

To write to the standard error, we can use the "Write" or "WriteLine" methods from the "Console" class, passing in the string we want to output. For example:

```C#
string error = "This is an error message"; //create error message
Console.Error.WriteLine(error); //write error message to stderr
```

This will display the "This is an error message" string in the console, with a red color to differentiate it from normal output. We can also use formatting to make the error message more informative, such as adding variable values to the string.

```C#
int number = 5; //create a variable
string error = $"Variable value is {number}"; //create error message with variable value
Console.Error.WriteLine(error); //write error message to stderr
```

The output of this code would be "Variable value is 5" in red. This is especially useful when dealing with complex error messages or multiple variables.

## Deep Dive
While writing to standard error may seem straightforward, there are a few things to keep in mind. First, it is important to note that the standard error stream is only used for error messages, not warnings or general output. This is to maintain the distinction between normal and error messages.

Additionally, when writing to standard error, the error message will appear in the console, but it will also be logged to any logging mechanisms being used for the program. This will make it easier to track and analyze errors later on.

Finally, it is good practice to always include informative and specific error messages when writing to standard error. This will make debugging and troubleshooting much easier, as well as providing helpful information for end-users if they encounter errors.

## See Also
For more information on streams and writing to different outputs, check out the following links:
- [Stream Class - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.stream)
- [Console Class - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.console)
- [Logging in C# - C# Station](http://csharpstation.com/TextFiles/Logging.aspx)