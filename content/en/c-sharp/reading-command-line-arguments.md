---
title:    "C# recipe: Reading command line arguments"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Why
As a programmer, it's important to have a thorough understanding of different concepts and techniques in order to write efficient and effective code. One such concept is working with command line arguments, which allows us to pass values to our program while it's being executed. In this blog post, we'll dive into the world of command line arguments in C#, exploring why it's important to know how to read them and how to do so effectively.

## How To
First and foremost, we need to understand the syntax of command line arguments. They are passed to our program as strings, separated by spaces. For example, if we were to run our program with the following command line arguments: "John 25 male", we would have three arguments: "John", "25", and "male". 

To read these arguments, we need to use the `args` parameter in the `Main()` method, which is the entry point of our program. It's an array of strings that contains all the command line arguments. Let's take a look at a simple example:

```C#
static void Main(string[] args)
{
    Console.WriteLine(args[0]); // Output: John
    Console.WriteLine(args[1]); // Output: 25
    Console.WriteLine(args[2]); // Output: male
}
```

We can also use a `foreach` loop to iterate through all the arguments, like this:

```C#
static void Main(string[] args)
{
    foreach (string arg in args)
    {
        Console.WriteLine(arg);
    }
}
```

This will print out all the arguments passed to our program in separate lines. Now, what if we want to pass in a different number of arguments each time we run our program? We can use the `Length` property of the `args` array to dynamically handle this, like so:

```C#
static void Main(string[] args)
{
    for (int i = 0; i < args.Length; i++)
    {
        Console.WriteLine($"Argument {i}: {args[i]}");
    }
}
```

This will print out the index of each argument along with its value. 

## Deep Dive
Now that we know how to read command line arguments, let's take a deeper dive into some more advanced concepts. One thing to keep in mind is that command line arguments are always passed as strings, which means we need to use data type conversion to perform any mathematical or logical operations on them. 

Another important point to note is the order in which arguments are passed. They are parsed from left to right, which means the first argument will be stored in `args[0]`, the second in `args[1]`, and so on. We can also use quotation marks to avoid spaces within an argument, for example: "John Doe" will be recognized as a single argument rather than two separate ones. 

Lastly, we can also use optional parameters in our command line arguments, by using a dash (`-`) followed by the parameter name and its corresponding value. These can be helpful for passing in specific options while running our program.

## See Also
- [Microsoft Docs: Main method](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/)
- [C# Corner: Working with Command-Line Arguments in C#](https://www.c-sharpcorner.com/article/working-with-command-line-arguments-in-c-sharp/)