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

## Why

Command line arguments are an essential part of any command line application. They allow us to pass in parameters or options when running a program, giving us more control and flexibility over how the program behaves. In this article, we will explore how to read command line arguments in C# and dive deeper into its inner workings.

## How To

Reading command line arguments in C# is a straightforward process. First, we need to access the arguments passed to our program using the `args` parameter in the `Main()` method. This parameter is an array of strings containing all the arguments passed in.

Let's take a look at a simple example where we want to print out the first argument passed to our program:

```C#
static void Main(string[] args)
{
    Console.WriteLine(args[0]);
}
```

If we run this program with a command line argument, such as `dotnet program.exe hello`, the output would be `hello`.

We can also use a loop to iterate through all the arguments passed in and perform any desired operations. For example, we may want to sum up all the numerical arguments passed in:

```C#
static void Main(string[] args)
{
    int sum = 0;
    for (int i = 0; i < args.Length; i++)
    {
        if (Int32.TryParse(args[i], out int num)) //checks if argument is a number
        {
            sum += num;
        }
    }
    Console.WriteLine($"The sum of all numerical arguments is {sum}");
}
```

If we run this program with arguments `dotnet program.exe 1 2 3 4 5`, the output would be `The sum of all numerical arguments is 15`.

We can also use command line arguments to provide options to our program. For example, we can use the built-in `Contains()` method to check if an argument contains a particular string:

```C#
static void Main(string[] args)
{
    if (args[0].Contains("--help"))
    {
        Console.WriteLine("Welcome to the help menu");
    }
}
```

Running this program with `dotnet program.exe --help` will trigger the `if` statement and print out the help menu.

## Deep Dive

Under the hood, command line arguments are nothing but an array of strings stored in the `args` parameter. This gives us the flexibility to read and manipulate the arguments as we see fit. However, there are a few things to keep in mind when working with command line arguments in C#:

- The first argument, `args[0]`, is always the program name or path.
- If an argument contains spaces, it needs to be wrapped in double quotes ("). For example, `dotnet program.exe "hello world"` would have two arguments: `args[0]` being the program name and `args[1]` being `hello world`.
- All arguments passed in are treated as strings, meaning we need to perform type conversion if we want to use the arguments numerically.
- We can use built-in methods such as `Contains()` or `StartsWith()` to check for specific options or parameters within the arguments passed in.

By keeping these points in mind, we can work with command line arguments efficiently and effectively in our C# programs.

## See Also

- [Microsoft's official documentation on command-line arguments in .NET](https://docs.microsoft.com/en-us/dotnet/api/system.environment.getcommandlineargs?view=net-5.0)
- [Exploring Command Line Arguments in C#](https://www.codeproject.com/Articles/3111/Exploring-Command-Line-Arguments-in-C) by Kavitha Subramaniyan
- [Parsing Command-line arguments using CommandLineApplication in .NET Core](https://www.kodoti.com/c-sharp/commandlineapplication) by Anton Angelov