---
title:                "Reading command line arguments"
html_title:           "C# recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments is a way for programmers to pass input values to their program without having to manually enter them during runtime. This is particularly useful for programs that need specific information to run, such as file paths or options for execution.

## How to:

To read command line arguments in C#, use the ```args``` parameter in the ```Main``` method, which is automatically generated when creating a console application. This parameter is of type ```string[]```, which means it is an array of strings that contains all the arguments passed to the program.

Example code:

```C#
static void Main(string[] args)
{
    Console.WriteLine("First argument: " + args[0]);
    Console.WriteLine("Second argument: " + args[1]);
}
```

Sample output:

```
> dotnet run argument1 argument2

First argument: argument1
Second argument: argument2
```

Note that the program name itself is also considered an argument, so the first element in the ```args``` array will always be the program name.

## Deep Dive:

Command line arguments have been a part of computer programming since the earliest days. In the early days of computing, computers only had a command line interface and programs were executed by typing in command line arguments. As graphical user interfaces became the norm, command line arguments became less prominent but are still used in various scenarios, such as software installation or batch processing.

In addition to reading command line arguments in the ```Main``` method, you can also use the ```Environment.GetCommandLineArgs()``` method to retrieve all the arguments as an array of strings. This method includes the program name as the first element in the array, similar to the ```args``` parameter in the ```Main``` method.

## See Also:

To learn more about using command line arguments in C#, check out the official Microsoft documentation here: [Command-Line Arguments (C# Programming Guide)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)