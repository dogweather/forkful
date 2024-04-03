---
date: 2024-01-30 18:57:17.219485-07:00
description: "Associative arrays, or what Rustaceans call \"hash maps,\" are collections\
  \ that store data in key-value pairs. Programmers use them for quick data lookup,\u2026"
lastmod: '2024-03-13T22:44:59.889352-06:00'
model: gpt-4-0125-preview
summary: Associative arrays, or what Rustaceans call "hash maps," are collections
  that store data in key-value pairs.
title: Using associative arrays
weight: 15
---

## How to:
In Rust, the `HashMap` type from the `std::collections` module provides the functionality of associative arrays. Here's how you can work with them:

```Rust
use std::collections::HashMap;

fn main() {
    // Creating a new HashMap
    let mut scores = HashMap::new();

    // Inserting values
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    // Accessing values
    let team_name = String::from("Blue");
    if let Some(score) = scores.get(&team_name) {
        println!("Score for team Blue: {}", score); // Output: Score for team Blue: 10
    }

    // Updating a value
    scores.entry(String::from("Blue")).and_modify(|e| *e += 5);

    // Iterating over key-value pairs
    for (key, value) in &scores {
        println!("{}: {}", key, value); // Output: Blue: 15, Yellow: 50
    }
}
```

## Deep Dive
The `HashMap` in Rust uses a hashing function to map keys to values, which enables rapid data retrieval. However, this efficiency comes with a cost: hash maps do not maintain the order of their elements. This is in contrast to other associative arrays implementations, like those in Python (`dict`) or Ruby, which as of recent versions maintain insertion order as a feature. For use cases where the order of key-value pairs is significant, Rust developers might consider using the `BTreeMap` from the `std::collections` module, which maintains order but might offer slower insertion and retrieval compared to `HashMap`. Ultimately, the choice between `HashMap` and `BTreeMap` depends on specific requirements around ordering and performance.
