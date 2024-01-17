---
title:                "Working with json"
html_title:           "C# recipe: Working with json"
simple_title:         "Working with json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Working with JSON is a way for programmers to transfer and store data in a lightweight and easy-to-read format. It stands for JavaScript Object Notation and is commonly used for data interchange between web applications. JSON's simplicity and flexibility make it a popular choice for data transfer, storage, and retrieval.

## How to:
To work with JSON in C#, you can use the Newtonsoft.Json library. First, install the library using NuGet by running the following command in the Package Manager Console:
```C#
Install-Package Newtonsoft.Json
```
Next, add a reference to the library in your C# code:
```C#
using Newtonsoft.Json;
```
To serialize an object to JSON, use the ```JsonConvert.SerializeObject()``` method:
```C#
var person = new { Name = "John", Age = 30 };

string json = JsonConvert.SerializeObject(person);

Console.WriteLine(json);

// Output: {"Name":"John","Age":30}
```

To deserialize JSON into an object, use the ```JsonConvert.DeserializeObject()``` method:
```C#
var json = @"{ 'Name': 'Jane', 'Age': 25 }";
var person = JsonConvert.DeserializeObject<Person>(json);
Console.WriteLine(person.Name); // Output: Jane
Console.WriteLine(person.Age); // Output: 25

// Define a class that matches the JSON key-value pairs
public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
}
```

## Deep Dive:
JSON was first introduced in 1999 as an alternative to the more complex XML format. It gained popularity with the rise of web APIs and is now the preferred format for data transfer in most web applications. Some alternatives to JSON include XML, CSV, and YAML, but JSON remains the most widely used due to its simplicity and wide support in programming languages.

The structure of JSON is similar to that of a JavaScript object, with key-value pairs enclosed in curly braces. Values can be strings, numbers, booleans, arrays, or other objects. In C#, JSON can be manipulated using classes or dynamic objects.

JSON is also human-readable, making it easy for developers to spot errors or debug data transfer issues. It is also supported by most modern web browsers and is a preferred data format for JavaScript developers.

## See Also:
- [Newtonsoft.Json library] (https://www.newtonsoft.com/json)
- [JSON.org] (https://www.json.org/)
- [JSON on Wikipedia] (https://en.wikipedia.org/wiki/JSON)