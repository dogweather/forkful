---
title:    "C# recipe: Reading command line arguments"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why
Before diving into the details of how to read command line arguments in C#, it's important to understand why this functionality is useful. Command line arguments allow developers to pass information to their program, enabling it to perform different operations based on the given input. This can be particularly helpful for automating tasks and making programs more versatile.

## How To
In C#, we can easily read command line arguments using the `Main()` method, which takes in an array of strings as a parameter. This array contains all of the arguments passed to the program when it is executed. We can then access these arguments by indexing the array, starting at 0.

Let's take a look at an example:

```C#
static void Main(string[] args)
{
    // check if there are any arguments passed
    if(args.Length > 0)
    {
        // access the first argument using index 0
        string argument = args[0];
        Console.WriteLine("The first argument is: " + argument);
    }
    else
    {
        Console.WriteLine("No arguments were passed.");
    }
}
```

If we were to run this code with the argument "hello" in the command line, the output would be: `The first argument is: hello`.

## Deep Dive
In addition to accessing the arguments themselves, we can also perform additional operations on them. For example, we can convert the arguments from strings to other data types, such as integers or booleans, using methods like `int.Parse()` or `bool.Parse()`. This allows us to validate and use the arguments in a more precise manner.

It's also important to note that command line arguments can be used in conjunction with other features in C#. For instance, we can use conditional statements to check for specific arguments and execute different blocks of code based on those arguments.

## See Also
To learn more about reading command line arguments in C#, check out these resources:
- [Microsoft Docs - Reading Command-Line Arguments](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [C# Corner - A Beginner's Guide to Command-line Arguments in C#](https://www.c-sharpcorner.com/article/a-beginners-guide-to-command-line-arguments-in-C-Sharp/)
- [Pluralsight - Working with Command-Line Arguments in C#](https://www.pluralsight.com/guides/working-with-command-line-arguments-in-c-sharp)