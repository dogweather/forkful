---
title:                "Using associative arrays"
date:                  2024-02-01T13:42:03.093659-07:00
model:                 gpt-4-0125-preview
simple_title:         "Using associative arrays"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/using-associative-arrays.md"
---

{{< edit_this_page >}}

## What & Why?

Associative arrays in Google Apps Script, essentially objects in JavaScript, let you store data as key-value pairs. They're the go-to when you need to organize data in a way that's easily accessible by a unique identifier, making your code cleaner and more efficient.

## How to:

Google Apps Script doesn't have "associative arrays" like some other languages. Instead, we use objects. Here’s how to get your feet wet:

```Google Apps Script
// Creating an associative array (object) with initial key-value pairs
var userScores = {
  "Alice": 23,
  "Bob": 35,
  "Charlie": 40
};

// Adding a new key-value pair
userScores["Diana"] = 28;

// Accessing a value using a key
Logger.log(userScores["Alice"]); // Output: 23

// Iterating over key-value pairs
for (var user in userScores) {
  Logger.log(user + ": " + userScores[user]);
  // Example Output: Alice: 23
  //                  Bob: 35
  //                  Charlie: 40
  //                  Diana: 28
}

// Deleting a key-value pair
delete userScores["Bob"];

// Check the resulting object
Logger.log(userScores);
// Example Output: {Alice=23, Charlie=40, Diana=28}
```

## Deep Dive

JavaScript (and by extension, Google Apps Script) does not use associative arrays in the traditional sense. Instead, objects play the role of associative arrays, mapping keys to values. This implementation is both powerful and flexible, allowing for complex data structures beyond simple key-value pairs.

One vital point to consider is that JavaScript objects only support strings and symbols as keys. If you're coming from a language that allows any data type as a key, this might require some adjustment in your thinking and approach.

While objects are incredibly useful for many tasks, if you need to maintain order or have complex data relationship needs, Maps or Arrays of objects might be a better choice. JavaScript `Map` object, for example, maintains key order and accepts various data types as keys, providing more flexibility than using an object for associative array-like behavior.

In sum, while Google Apps Script doesn't natively support associative arrays as distinct types, its use of objects (and to a more advanced extent, Maps) for this purpose offers a robust and flexible solution. However, depending on your specific needs, exploring other data structures might yield more optimal results.
