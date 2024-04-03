---
date: 2024-01-30 18:57:29.712638-07:00
description: "In Java, associative arrays, or maps, let you store key-value pairs\
  \ for efficient data lookup and manipulation. Programmers use them for tasks like\u2026"
lastmod: '2024-03-13T22:44:59.966220-06:00'
model: gpt-4-0125-preview
summary: In Java, associative arrays, or maps, let you store key-value pairs for efficient
  data lookup and manipulation.
title: Using associative arrays
weight: 15
---

## What & Why?

In Java, associative arrays, or maps, let you store key-value pairs for efficient data lookup and manipulation. Programmers use them for tasks like counting occurrences of items or mapping users to their permissions because they offer speedy access and updates.

## How to:

Java doesn't have built-in associative arrays like some languages do, but it provides the `Map` interface and classes like `HashMap` and `TreeMap` to fill that role. Here’s how to use a `HashMap`:

```Java
import java.util.HashMap;
import java.util.Map;

public class LearnMaps {
    public static void main(String[] args) {
        // Creating a HashMap
        Map<String, Integer> ageOfFriends = new HashMap<>();
        
        // Adding elements
        ageOfFriends.put("Alice", 24);
        ageOfFriends.put("Bob", 30);
        ageOfFriends.put("Charlie", 28);

        // Accessing elements
        System.out.println("Alice's age: " + ageOfFriends.get("Alice"));
        
        // Handling non-existent keys
        System.out.println("Age of someone not in the map: " + ageOfFriends.getOrDefault("Dan", -1));

        // Iterating over elements
        for (Map.Entry<String, Integer> entry : ageOfFriends.entrySet()) {
            System.out.println(entry.getKey() + " is " + entry.getValue() + " years old.");
        }
    }
}
```

Sample Output:

```
Alice's age: 24
Age of someone not in the map: -1
Alice is 24 years old.
Bob is 30 years old.
Charlie is 28 years old.
```

`HashMap` is only one implementation. If your keys are unique and you need them sorted, consider `TreeMap`. For a map that retains order of insertion, `LinkedHashMap` is your friend.

## Deep Dive

Maps in Java are part of the Collections Framework, introduced in JDK 1.2, but have seen significant improvements over the years, including the introduction of the `forEach` method in Java 8 for easier iteration over entries. The choice of map implementation (`HashMap`, `LinkedHashMap`, `TreeMap`) should be dictated by your specific needs in terms of ordering and performance. For instance, `HashMap` offers O(1) time performance for the basic operations (get and put), assuming the hash function disperses the elements properly among the buckets. However, if you need sorting based on natural ordering or custom comparators, `TreeMap` is the go-to, providing O(log n) time for insertion and lookup.

Before `Map` was introduced, associative arrays were usually implemented with two parallel arrays (one for keys, one for values) or custom data structures with less efficiency. Current alternatives to `Map` and its implementations could include third-party libraries offering specialized maps, such as bidirectional maps (BiMap in Google's Guava library) for cases where you need to find a key by its value efficiently. However, for most use cases in Java, the standard library's maps are robust and flexible enough to handle the task.
