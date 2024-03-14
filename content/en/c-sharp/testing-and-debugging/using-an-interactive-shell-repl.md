---
date: 2024-01-25 03:39:45.126377-07:00
description: "A REPL, or Read-Eval-Print Loop, lets you type C# code and run it interactively.\
  \ Programmers use it for quick experiments, debugging, or learning C#,\u2026"
lastmod: '2024-03-13T22:45:00.090500-06:00'
model: gpt-4-1106-preview
summary: "A REPL, or Read-Eval-Print Loop, lets you type C# code and run it interactively.\
  \ Programmers use it for quick experiments, debugging, or learning C#,\u2026"
title: Using an interactive shell (REPL)
---

{{< edit_this_page >}}

## What & Why?
A REPL, or Read-Eval-Print Loop, lets you type C# code and run it interactively. Programmers use it for quick experiments, debugging, or learning C#, without the overhead of setting up full projects.

## How to:
Fire up a REPL in your C# environment using the C# Interactive window or run `dotnet-script` in your terminal. Here's a taste of using it:

```csharp
> var greeting = "Hello, REPL!";
> Console.WriteLine(greeting);
Hello, REPL!
> 
```

You instantly get feedback. No compile and run steps. Just code and see.

## Deep Dive
REPL journeyed from Lisp to modern languages, thriving in dynamic ones like Python. With C#, Roslyn brought the REPL closer to developers. `csi` for Roslyn, and `dotnet-script` for .NET Core, are solid options. A deeper cut: they evaluate code per line, not all together, a different execution model versus typical C# apps. This impacts state persistence across executions and the scope of variables.

Visual Studio's C# Interactive window is a REPL powered by Roslyn. It has Intellisense, multiple references, and NuGet package support. Quite a step up from early command line experiments.

For alternative languages, Python uses `IDLE`, JavaScript has Node.js's REPL, and F# ships with `F# Interactive`. Each fosters instant feedback loops, invaluable for testing small code snippets or understanding language features.

## See Also
- [.NET Core `dotnet-script` REPL](https://github.com/filipw/dotnet-script)
