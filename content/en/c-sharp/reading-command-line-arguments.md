---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments in C# involves retrieving the values passed to a program at the start from the command line. Developers use this feature to allow user input to dictate the operation of programs, providing a flexible way to control application behavior without modifying the code.

## How to:

Here's a simple C# code snippet that reads command line arguments:

```C#
class Program {
    static void Main(string[] args) {
        for (int i = 0; i < args.Length; i++) {
            Console.WriteLine("Arg{0} = {1}", i, args[i]);
        }
    }
}
```

Assuming you run your program `program.exe` with two arguments "hello" and "world", the output would be:

```sh
Arg0 = hello
Arg1 = world
```

## Deep Dive

Historically, command line arguments were the primary way to pass parameters to a program. They're still useful for scripts, batch operations, and tools.

As alternatives, consider using configuration files or environment variables, which can be more suitable for complex parameters or sensitive data.

When reading command line arguments in C#, the arguments are stored in the `args` array passed to the `Main` method. It doesn't contain the program name, contrary to some languages like C or C++. The array is automatically populated by C# runtime, and arguments passed on command line are separated by spaces, unless they're enclosed in double quotes - these will be treated as a single argument.

## See Also

1. More on command-line arguments in .NET: [https://docs.microsoft.com/en-us/dotnet/core/tutorials/cmdline](https://docs.microsoft.com/en-us/dotnet/core/tutorials/cmdline)
2. Other ways to provide app configuration: [https://docs.microsoft.com/en-us/aspnet/core/fundamentals/configuration](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/configuration)
3. For understanding the C# Main method: [https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/).