---
date: 2024-01-30 18:57:11.420009-07:00
description: 'How to: In C#, you work with associative arrays using the `Dictionary<TKey,
  TValue>` class. Here''s a quick example to get you started.'
lastmod: '2024-03-13T22:45:00.081621-06:00'
model: gpt-4-0125-preview
summary: In C#, you work with associative arrays using the `Dictionary<TKey, TValue>`
  class.
title: Using associative arrays
weight: 15
---

## How to:
In C#, you work with associative arrays using the `Dictionary<TKey, TValue>` class. Here's a quick example to get you started:

```C#
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        // Creating a dictionary
        Dictionary<string, int> fruitBasket = new Dictionary<string, int>();

        // Adding key-value pairs
        fruitBasket.Add("Apples", 5);
        fruitBasket.Add("Oranges", 10);

        // Accessing a value using its key
        Console.WriteLine("Apples: " + fruitBasket["Apples"]);
        
        // Updating a value
        fruitBasket["Apples"] = 7;
        Console.WriteLine("Updated Apples: " + fruitBasket["Apples"]);
        
        // Removing a key-value pair
        fruitBasket.Remove("Oranges");

        // Iterating over the dictionary
        foreach (var pair in fruitBasket)
        {
            Console.WriteLine(pair.Key + ": " + pair.Value);
        }
    }
}
```
Sample Output:
```
Apples: 5
Updated Apples: 7
Apples: 7
```

This example showcases creating a dictionary, adding, accessing, updating, and removing elements, and iterating over it.

## Deep Dive
The concept of associative arrays goes back to their use in scripting languages like Perl and PHP, where they offer flexibility in managing collections of data. In C#, `Dictionary<TKey, TValue>` is the de facto implementation, introduced in .NET Framework 2.0. It stores data in a hash table, ensuring efficient look-ups, additions, and deletions.

However, it's worth noting that while dictionaries are incredibly versatile, they may not always be your best bet. For maintaining ordered collections, you might look into `SortedDictionary<TKey, TValue>` or `SortedList<TKey, TValue>`, which offer sorted order at the cost of slower insertion and removal operations. For scenarios demanding thread-safety, `ConcurrentDictionary<TKey, TValue>` adds overhead but ensures safe access from multiple threads without manual locking.

Ultimately, the choice of an associative array implementation in C# hinges on your specific needs regarding order, performance, and thread safety.
