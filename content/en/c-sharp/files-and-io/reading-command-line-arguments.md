---
date: 2024-01-20 17:55:43.038233-07:00
description: "Reading command line arguments allows a C# program to process user inputs\
  \ given at launch. Programmers use this to customize app behavior without altering\u2026"
lastmod: '2024-03-13T22:45:00.103208-06:00'
model: gpt-4-1106-preview
summary: Reading command line arguments allows a C# program to process user inputs
  given at launch.
title: Reading command line arguments
weight: 23
---

## What & Why?
Reading command line arguments allows a C# program to process user inputs given at launch. Programmers use this to customize app behavior without altering code.

## How to:
Here's how to gobble up those command line arguments:

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("You've entered the following arguments:");
        foreach (string arg in args)
        {
            Console.WriteLine(arg);
        }
    }
}
```

If you run your program like this: `yourapp.exe arg1 arg2 arg3`, expect the output:

```
You've entered the following arguments:
arg1
arg2
arg3
```

## Deep Dive
The tradition of command line arguments harks back to the dawn of computing, allowing early software to be flexible. In C#, `args` is a string array in `Main()` holding the arguments passed. Alternatives? Sure, there are libraries such as `CommandLineParser` that beef up capabilities, but for many tasks, `args` is your quick and dirty friend.

Under the hood, a C# app starts with `Main()`. When you call your app from a command line or script, the operating system slaps the arguments into an array and passes it to `Main()`. Easy peasy.

Got a complex app? Maybe you need to parse flags, options and values? That's where libs shine with more control and less boilerplate code than raw `args` parsing. But for simple input? `args` all the way.

## See Also
- [Microsoft Docs on Main() and command-line arguments](https://docs.microsoft.com/en-us/dotnet/csharp/fundamentals/program-structure/main-command-line)
- [CommandLineParser library on GitHub](https://github.com/commandlineparser/commandline)
- [Stack Overflow discussion on parsing command line arguments in C#](https://stackoverflow.com/questions/491595/best-way-to-parse-command-line-arguments-in-c)
