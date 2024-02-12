---
title:                "Refactoring"
aliases: - /en/c-sharp/refactoring.md
date:                  2024-01-25T02:12:43.594010-07:00
model:                 gpt-4-1106-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/refactoring.md"
---

{{< edit_this_page >}}

## What & Why?

Refactoring is the process of restructuring existing computer code without changing its external behavior. Programmers do it to clean up code, enhance readability, reduce complexity, and improve maintainability.

## How to:

Let's refactor a simple C# method that calculates and prints the sum of an array of numbers:

Before Refactoring:
```C#
public class Calculator
{
    public void CalculateSum()
    {
        int[] numbers = { 1, 2, 3, 4, 5 };
        int sum = 0;
        for (int i = 0; i < numbers.Length; i++)
        {
            sum += numbers[i];
        }
        Console.WriteLine("The sum is " + sum);
    }
}
```

After Refactoring:
```C#
public class Calculator
{
    private readonly int[] _numbers;

    public Calculator(int[] numbers)
    {
        _numbers = numbers;
    }

    public int CalculateSum()
    {
        return _numbers.Sum();
    }

    public void DisplaySum()
    {
        Console.WriteLine($"The sum is {CalculateSum()}");
    }
}

// Usage:
var calculator = new Calculator(new[] { 1, 2, 3, 4, 5 });
calculator.DisplaySum();
```

By refactoring, we've separated concerns, made the `Calculator` class more flexible by allowing it to take any array of numbers, and leveraged LINQ to make the sum calculation more concise.

## Deep Dive

Refactoring has its roots in the smalltalk programming community and was popularized in the 1990s by Martin Fowler's book "Refactoring: Improving the Design of Existing Code". Over the years, it's become a fundamental part of agile methodologies and good coding practices.

There are various approaches to refactoring, such as Red-Green-Refactor in Test-Driven Development (TDD). It ensures that refactoring doesn't introduce bugs by starting with a failing test, making it pass, and then cleaning up the code.

When implementing refactoring, it's crucial to have a comprehensive test suite to ensure that no functionality gets broken during the process. Automated refactoring tools, like ReSharper for C#, can also aid in this process by providing safe ways to change code structures. However, tooling should be supplementary to a deep understanding of the codebase and coding principles.

## See Also

- Martin Fowler's seminal work on Refactoring: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- Microsoft's guide on Refactoring in Visual Studio: [Refactoring (C#)](https://docs.microsoft.com/en-us/visualstudio/ide/refactoring-in-visual-studio?view=vs-2022)
- A detailed look into Refactoring patterns with examples: [SourceMaking Refactoring](https://sourcemaking.com/refactoring)
