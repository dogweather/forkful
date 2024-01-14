---
title:    "C# recipe: Printing debug output"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of programming, and one of the most common methods to assist in this process is by using print statements to output information. It may seem simple, but understanding how to effectively print debug output can greatly improve the efficiency and accuracy of your code.

## How To

Printing debug output in C# is a straightforward process using the `Console` class. First, we need to import the `System` namespace in our code. Then, we can use the `WriteLine()` method to display text in the console. Let's take a look at a sample code:

```C#
using System;

class Program
{
    static void Main()
    {
        string name = "John";
        int age = 30;
        Console.WriteLine("Name: " + name);
        Console.WriteLine("Age: " + age);
    }
}
```

In this example, we create a variable called `name` and assign it a value of "John". We also have another variable called `age` with a value of 30. By using the `Console.WriteLine()` method, we can output the values of these variables in the console. The output will be:

```
Name: John
Age: 30
```

This allows us to quickly check the values of our variables and see if they are correct, without disrupting the flow of our program.

## Deep Dive

Printing debug output goes beyond just displaying values of variables. It can also be used to track the flow of your program and identify any errors. For example, let's say we have a function that calculates the average of two numbers:

```C#
static double CalculateAverage(double num1, double num2)
{
    double average = (num1 + num2) / 2;
    Console.WriteLine("Calculating average...");
    Console.WriteLine("First number: " + num1);
    Console.WriteLine("Second number: " + num2);
    Console.WriteLine("Average: " + average);
    return average;
}
```

By adding these debug output statements, we can not only see the output of our function, but also the values of the parameters and the calculated average. This can be extremely helpful in identifying any errors and understanding the logic of our code.

## See Also

Here are some helpful resources to further explore printing debug output in C#:

- [Microsoft Docs - How to: Debug in Visual Studio](https://docs.microsoft.com/en-us/visualstudio/debugger/debugger-feature-tour?view=vs-2019)
- [Stack Overflow - What is debugging?](https://stackoverflow.com/questions/217187/what-is-debugging)
- [C# Programming Guide - Using the Console class](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/using-the-console-class)

Now that you know the basics of printing debug output, go ahead and try it out in your own code. Happy debugging!