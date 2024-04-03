---
date: 2024-01-25 02:59:35.207552-07:00
description: "Chunking code into functions is like sorting LEGO bricks into bins\u2014\
  it makes finding and using them easier. We do this to avoid repetition, to simplify\u2026"
lastmod: '2024-03-13T22:45:00.094340-06:00'
model: gpt-4-1106-preview
summary: "Chunking code into functions is like sorting LEGO bricks into bins\u2014\
  it makes finding and using them easier."
title: Organizing code into functions
weight: 18
---

## What & Why?
Chunking code into functions is like sorting LEGO bricks into bins—it makes finding and using them easier. We do this to avoid repetition, to simplify understanding, and to make maintenance less of a headache.

## How to:
Imagine you've got code that prints a greeting several times. Without functions, it's a mess. With functions, it's neat.

```C#
// Without functions - repetitive
Console.WriteLine("Hello, Amy!");
Console.WriteLine("Hello, Bob!");
Console.WriteLine("Hello, Charlie!");

// With functions - cleaner
void Greet(string name) {
    Console.WriteLine($"Hello, {name}!");
}

Greet("Amy");
Greet("Bob");
Greet("Charlie");
```

Output is the same, but the second version is much tidier.

## Deep Dive
Way back, in assembly language days, you'd jump to different code spots with GOTO—chaotic and hard to track. Functions are a major level-up, like organized drawers in a toolbox. Alternatives? Sure. You've got methods, which are functions in a class context. Then there are lambdas and inline functions for quick, one-off tasks.

About implementation—small, focused functions are gold. They’re easier to test and debug. Large functions with many responsibilities can become monstrous, earning the dubious title "spaghetti code". Stick to one job per function; you'll thank yourself later.

## See Also
For more on functions and best practices, check out:

- Clean Code by Robert C. Martin: Principles to keep your functions tidy.
- Refactoring by Martin Fowler: Ways to improve existing code.
- Microsoft C# Guide on Methods: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/methods
