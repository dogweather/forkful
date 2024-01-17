---
title:                "Printing debug output"
html_title:           "C# recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Debugging is an essential part of programming, and one of the most common ways to identify and fix errors is by printing out debug output. This is the process of displaying specific information about the program's execution at various stages. Programmers use this technique to understand how their code is running, detect any unexpected behavior, and ultimately, troubleshoot and resolve issues.

## How to:
In C#, you can print debug output to the console window using the `Console.WriteLine()` method. This method takes in a string or variable and displays it in the console. Let's see an example:

```C#
int num1 = 10;
int num2 = 5;

int sum = num1 + num2;

Console.WriteLine("The sum of {0} and {1} is {2}.", num1, num2, sum);
```

Output:
```
The sum of 10 and 5 is 15.
```
In this example, we have declared two integer variables, `num1` and `num2`, and assigned them values. Then, we have calculated the sum and used `Console.WriteLine()` to display it, along with the value of the two variables.

You can also use string interpolation to print out debug output. String interpolation allows you to embed expressions directly into the string, making your code more readable and concise. Here's an example using the same scenario as above:
```C#
int num1 = 10;
int num2 = 5;

int sum = num1 + num2;

Console.WriteLine($"The sum of {num1} and {num2} is {sum}.");
```

Output:
```
The sum of 10 and 5 is 15.
```

## Deep Dive:
Printing debug output is not a new concept, and it has been an essential tool for programmers throughout the years. Before modern programming languages, developers used to write their debug output on a piece of paper or use a dedicated debugging tool to display the information. But as we progressed towards more advanced programming languages, printing debug output using `Console.WriteLine()` became the norm.

There are other ways to print out debug information, such as using a debugger or logging library. These tools allow developers to monitor their code's execution step-by-step to identify and resolve any bugs. However, printing debug output to the console remains the go-to solution for quick troubleshooting.

## See Also:
- [Debugging in C#](https://docs.microsoft.com/en-us/visualstudio/debugger/debugging-csharp?view=vs-2019)
- [C# String Interpolation](https://docs.microsoft.com/en-us/dotnet/csharp/tutorials/string-interpolation)
- [C# Logging Frameworks](https://stackify.com/best-csharp-logging-frameworks/)