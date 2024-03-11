---
date: 2024-02-25 17:04:52.754084-07:00
description: "String interpolation in C# allows you to create a new string by including\
  \ expressions inside a string literal, making it easier to format and concatenate\u2026"
lastmod: '2024-03-11T00:14:33.939802-06:00'
model: gpt-4-0125-preview
summary: "String interpolation in C# allows you to create a new string by including\
  \ expressions inside a string literal, making it easier to format and concatenate\u2026"
title: Interpolating a string
---

{{< edit_this_page >}}

## What & Why?
String interpolation in C# allows you to create a new string by including expressions inside a string literal, making it easier to format and concatenate strings. Programmers use this feature to improve code readability and maintainability, especially when dealing with dynamic string content.

## How to:
In C#, string interpolation is denoted by a dollar sign (`$`) followed by a string literal. The variable names or expressions are enclosed within curly braces (`{}`).

```csharp
string name = "Jane";
int age = 28;
string interpolatedString = $"Hello, {name}! You are {age} years old.";
Console.WriteLine(interpolatedString);
// Output: Hello, Jane! You are 28 years old.
```

In a more complex example, you can perform operations or call methods within the curly braces:

```csharp
double price = 19.99;
int quantity = 3;
string orderDetail = $"Total price: {price * quantity:C2}";
Console.WriteLine(orderDetail);
// Output: Total price: $59.97
```
The `:C2` format specifier inside the curly braces formats the number as a currency with two decimal places.

For scenarios requiring more advanced formatting or localization, you might consider using the `string.Format` method or libraries like Humanizer. Humanizer can manipulate and display strings, dates, times, timespans, numbers, and quantities in a more human-readable format. Below is an example of using Humanizer for complex string manipulation. Note that Humanizer is not part of the .NET standard library and requires installing the NuGet package `Humanizer`.

First, install Humanizer via NuGet:

```
Install-Package Humanizer
```

Then, you can use it as follows:

```csharp
using Humanizer;

int dayDifference = 5;
string humanized = $"The event was {dayDifference} days ago.".Humanize();
Console.WriteLine(humanized);
// Depending on the configuration and culture, a possible output: The event was 5 days ago.
```

This example demonstrates basic usage. Humanizer supports a broad range of functionalities that can be applied to strings, dates, numbers, and more, making your applications more accessible and intuitive.
