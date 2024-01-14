---
title:                "C# recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why 
Command line arguments are a powerful tool for developers to manipulate their programs without having to constantly modify the code. They allow for flexibility and customization, making the program more user-friendly. In this blog post, we'll explore how to effectively read command line arguments in C#.

## How To 
Reading command line arguments in C# is a simple process that can greatly enhance the functionality of your program. Let's take a look at an example code:

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        for (int i = 0; i < args.Length; i++)
        {
            Console.WriteLine($"Argument {i+1}: {args[i]}");
        }
    }
}
```

In this code, we first declare a `Main` method that takes in an array of strings called `args`, which represent the command line arguments. Then, using a `for` loop, we can iterate through the arguments and access each one individually. In this case, we print out the index and value of each argument using Console.WriteLine.

To run this program with command line arguments, we can navigate to the directory where the executable file is located and type in `program.exe arg1 arg2 arg3`. This will output:

```C#
Argument 1: arg1
Argument 2: arg2
Argument 3: arg3
```

Notice that the program reads the arguments in the same order they were inputted in the command line. This can be useful when passing in multiple parameters or options for your program.

## Deep Dive
Command line arguments can also be used to set values for variables in your program. For example, let's say we have a program that performs a calculation and we want to be able to specify the input values through command line arguments. We can do this by converting the argument values to the appropriate data type, using methods like `int.Parse()` or `double.Parse()`, and assigning them to our variables. This allows for more dynamic and customizable programs.

It's important to note that command line arguments are case-sensitive, meaning that `arg` and `Arg` would be treated as different arguments. Additionally, we can include special characters, such as spaces and symbols, in our arguments by using quotation marks ("") to encapsulate them.

There are also special characters that have a reserved function in command line arguments, such as the backslash (\) for escaping characters and the hyphen (-) for indicating an option.

## See Also 
- [Microsoft Docs on Command Line Arguments in C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [C# Command Line Parser Library](https://github.com/commandlineparser/commandline)
- [Passing Command Line Arguments to a Console Application](https://www.softfluent.com/blog/dev/Passing-command-lines-to-a-console-application)