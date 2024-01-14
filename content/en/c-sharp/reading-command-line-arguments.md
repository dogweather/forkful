---
title:                "C# recipe: Reading command line arguments"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why
Command line arguments may seem like a mundane topic, but they are actually an important aspect of programming. Understanding how to read and manipulate command line arguments can greatly improve the functionality and efficiency of your code. In this blog post, we will explore the use of command line arguments in C# and how they can benefit your programming journey.

## How To
In C#, reading command line arguments is a simple process. You start by creating a string array variable to store the arguments, which is automatically generated when the program is executed. Then, you can use a for loop to iterate through the array and access each individual argument.

```
C# // Example code to read command line arguments
static void Main(string[] args)
{
    for (int i = 0; i < args.Length; i++)
    {
        Console.WriteLine("Argument {0}: {1}", i + 1, args[i]);
    }
}
```

Let's say we have a program called "Hello" and we execute it with the following command line arguments: `Hello John 123`. The output would be:

```
Argument 1: John
Argument 2: 123
```

As you can see, the arguments are stored in the `args` array in the order they were entered in the command line. You can also use conditional statements or other logic to manipulate the arguments as needed for your program.

## Deep Dive
Now, let's dive a little deeper into the world of command line arguments. Did you know that you can pass in not only strings, but also numbers and even files? This allows for even more flexibility in your programs. You can also use special characters, such as quotes, to handle arguments with spaces or other special characters. For example, `Hello "John Smith" 123` would pass in "John Smith" as one argument instead of two separate arguments.

It's also important to note that the first argument in the array (`args[0]`) is always the name of the program itself. This can be useful for programs that need to reference themselves or if you want to provide instructions or options in the first argument.

## See Also
- [Microsoft Docs: Command-line arguments (C# Programming Guide)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [C# Corner: Command Line Arguments in C#](https://www.c-sharpcorner.com/article/command-line-arguments-in-C-Sharp)

With the knowledge and understanding of command line arguments in C#, you can greatly improve the functionality and user experience of your programs. Whether you're a beginner or an experienced programmer, the use of command line arguments is a skill worth adding to your repertoire. Happy coding!