---
title:                "Using associative arrays"
aliases: - /en/google-apps-script/using-associative-arrays.md
date:                  2024-02-01T21:12:00.039102-07:00
model:                 gpt-4-0125-preview
simple_title:         "Using associative arrays"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/using-associative-arrays.md"
---

{{< edit_this_page >}}

## What & Why?

Associative arrays, known as objects in Google Apps Script (a variant of JavaScript), allow programmers to create collections of key-value pairs. This functionality is pivotal for storing and manipulating data efficiently, especially when working with dynamically named properties or when a traditional array's linear storage and access model is insufficient.

## How to:

In Google Apps Script, you create and manipulate associative arrays (objects) using braces `{}`, defining key-value pairs within. Keys are unique identifiers, and values can be anything from strings and numbers to more complex objects or functions. Here's a basic example:

```javascript
function createAssociativeArray() {
  var user = {
    name: "John Doe",
    age: 30,
    email: "johndoe@example.com"
  };

  // Accessing values
  Logger.log(user.name); // Outputs: John Doe
  Logger.log(user["email"]); // Outputs: johndoe@example.com

  // Adding new key-value pairs
  user.title = "Software Developer";
  user["country"] = "USA";

  Logger.log(user.title); // Outputs: Software Developer

  // Iterating over key-value pairs
  for (var key in user) {
    Logger.log(key + ': ' + user[key]);
  }
}
```

Sample output for the iteration part might look like this:
```
name: John Doe
age: 30
email: johndoe@example.com
title: Software Developer
country: USA
```

Note how you can use both dot notation and bracket notation for accessing and setting properties. Bracket notation is particularly useful when working with keys that are dynamically determined or include characters not permissible in identifiers.

## Deep Dive

Associative arrays in form of objects have been a cornerstone of JavaScript, and by extension Google Apps Script, reflecting its prototype-based inheritance mechanism. Unlike languages with traditional associative arrays or dictionaries (e.g., Python's dict), Google Apps Script objects provide a flexible and powerful means to structure data, benefiting from JavaScript's dynamic nature.

It's important to note, however, that the ECMAScript 2015 specification introduced `Map` and `Set` objects, offering a more straightforward associative collection handling with certain benefits over objects, such as maintaining insertion order and better performance for large datasets. While Google Apps Script supports these as well, the choice between using objects or the newer `Map`/`Set` structures depends on specific needs and performance considerations. For most associative array tasks, traditional object-based implementations provide a familiar and versatile approach, but examining newer alternatives is advisable as your script's complexity grows.
