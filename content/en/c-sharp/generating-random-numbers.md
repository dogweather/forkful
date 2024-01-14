---
title:    "C# recipe: Generating random numbers"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Why
Random numbers are a vital component in many programming tasks, such as generating unique IDs, shuffling data, or incorporating randomness in simulations. In this blog post, we will delve into how to generate random numbers in C# and explore some underlying concepts.

## How To
In C#, the built-in `Random` class can be used to generate random numbers. Let's first create an instance of the `Random` class:

```C#
Random random = new Random();
```

This instance allows us to access the methods for generating random numbers. Here are some examples:

### Generating a random integer
To get a random integer within a specific range, we can use the `Next()` method, which takes two arguments: the inclusive lower bound and the exclusive upper bound. For example, to generate a random number between 1 and 10, we can use:

```C#
int num = random.Next(1, 11); // returns a number between 1 and 10
```

### Generating a random double
For generating a random `double` between 0 and 1, we can use the `NextDouble()` method:

```C#
double rand = random.NextDouble(); // returns a decimal value between 0 and 1
```

### Shuffling elements in a list
The `Random` class also provides a convenient `Next()` overload for shuffling elements in a `List<T>`. It takes no arguments and returns a random number that can be used as the index for shuffling. For example:

```C#
List<int> numbers = new List<int> { 1, 2, 3, 4, 5 };
numbers.OrderBy(x => random.Next()).ToList();
// this will shuffle the elements in the list
```

### Sample Output
To demonstrate the results of our random number generation, let's use the `Console` class to print out some examples:

```C#
int num1 = random.Next(1, 11); 
int num2 = random.Next(1, 11);
int num3 = random.Next(1, 11);
Console.WriteLine($"Three random numbers are: {num1}, {num2}, {num3}");
double rand1 = random.NextDouble();
double rand2 = random.NextDouble();
double rand3 = random.NextDouble();
Console.WriteLine($"Three random decimals are: {rand1}, {rand2}, {rand3}");
```

Output:
``` 
Three random numbers are: 3, 6, 9
Three random decimals are: 0.714601275, 0.029753846, 0.524246478
```

## Deep Dive
Behind the scenes, the `Random` class generates pseudorandom numbers based on a seed value. The seed value is used to initialize a mathematical algorithm that produces a sequence of numbers that appears random. Each time we create a new instance of `Random`, a different seed value is used, resulting in a different sequence of numbers.

### Controlling the seed value
If we want to have a predictable sequence of random numbers, we can pass in a specific seed value when creating our `Random` instance. This can be useful for testing or debugging purposes.

```C#
int seed = 12345; // seed value can be any integer
Random random = new Random(seed);
```

### Avoiding predictable patterns
Since the `Random` class generates pseudorandom numbers, there is a possibility of repeating patterns if the seed values are predictable. To avoid this, we can use a more sophisticated algorithm for generating random numbers, such as the `RNGCryptoServiceProvider` class.

## See Also
- [Microsoft Docs: Random Class](https://docs.microsoft.com/en-us/dotnet/api/system.random)
- [C# Random Numbers](https://www.c-sharpcorner.com/UploadFile/mahesh/RandomNumbers11142005003143AM/RandomNumbers.aspx)
- [Generating Random Numbers in C#](https://www.c-sharpcorner.com/blogs/magic-of-generating-random-numbers-in-c-sharp1)