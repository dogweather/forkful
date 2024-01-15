---
title:                "Working with json"
html_title:           "Javascript recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Why

JSON (JavaScript Object Notation) is a popular data interchange format in web development, making it essential to understand for any aspiring or experienced developer. It allows for the easy storage and sharing of data across different applications and systems, making it a valuable skill to have in your toolkit.

## How To

To begin working with JSON, we first need to understand its basic structure. It consists of key-value pairs, enclosed in curly braces `{}`, with each key and value separated by a colon `:`. For example, let's say we want to store some information about a person, we could create a JSON object like this:

```Javascript
let person = {
  "name": "John",
  "age": 28,
  "country": "USA"
}
```

Notice how the keys (name, age, country) are enclosed in double quotes, while the values (John, 28, USA) can be either strings or numbers. This is the most common way to create a JSON object, but it can also be created using a JSON JavaScript object constructor.

To access specific values in the JSON object, we use dot notation, just like we would with a regular JavaScript object. For example, to access the person's age, we would use `person.age`.

Now let's take a look at how we can convert a JavaScript object or array to a JSON string, using the `JSON.stringify()` method. This is useful when we want to send data to a server or store it in a file. For example:

```Javascript
let fruits = ['apple', 'banana', 'orange'];
let fruitsJSON = JSON.stringify(fruits);
console.log(fruitsJSON);
// Output: ["apple","banana","orange"]
```

Notice how the output is now a string, and the keys have double quotes around them. We can also convert a JSON string back to a JavaScript object using the `JSON.parse()` method. For example:

```Javascript
let fruitsJSON = '["apple","banana","orange"]';
let fruits = JSON.parse(fruitsJSON);
console.log(fruits);
// Output: ['apple', 'banana', 'orange']
```

## Deep Dive

One of the great things about JSON is its flexibility. It allows us to store different types of data in one object, making it easy to work with complex data structures. Nested objects and arrays can also be created within a JSON object, allowing for even more versatile data storage.

Another important aspect to understand is how JSON handles data types. JSON supports six data types: string, number, boolean, null, array, and object. However, it does not support functions, undefined, or dates, so we need to be conscious of this when working with it.

It's also worth noting that keys in a JSON object must be unique and follow the same naming rules as JavaScript objects (must be a string or valid identifier). Values can be of any of the supported data types.

Lastly, when comparing JSON objects, they are considered equal only if their key-value pairs are in the same order. This is why it's important to use the `JSON.stringify()` method when comparing two JSON objects.

## See Also

- [JSON Tutorial for Beginners](https://www.w3schools.com/js/js_json_intro.asp)
- [Understanding JSON in JavaScript](https://www.digitalocean.com/community/tutorials/understanding-json-in-javascript)
- [JSON Cheat Sheet](https://cheatography.com/karlredman/cheat-sheets/json-cheat-sheet/)