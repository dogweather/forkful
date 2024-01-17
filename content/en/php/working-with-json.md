---
title:                "Working with json"
html_title:           "PHP recipe: Working with json"
simple_title:         "Working with json"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON, or JavaScript Object Notation, is a lightweight data interchange format commonly used in web development. It is used to store and transmit data between a server and a web application, making it a crucial tool for developers who need to parse and handle data in their projects.

## How to:
To work with JSON in PHP, we can use the built-in functions `json_encode()` and `json_decode()`. Let's take a look at an example:

```
// Encoding data into JSON format
$data = array(
    'name' => 'John',
    'age' => 25,
    'hobbies' => array('reading', 'gaming', 'hiking')
);

$json = json_encode($data);

// Output: {"name":"John", "age":25, "hobbies":["reading","gaming","hiking"]}
echo $json;

// Decoding data from JSON format
$decoded_data = json_decode($json);

// Output: John
echo $decoded_data->name;

```

As we can see, the JSON data is converted into a string when using the `json_encode()` function and can be easily decoded back into a PHP object using `json_decode()`.

We can also pass an optional second argument to the `json_decode()` function to specify whether we want to get the data back as an object or an array. For example:

```
// Decoding data as an array
$decoded_data = json_decode($json, true);

// Output: John
echo $decoded_data['name'];
```

## Deep Dive:
JSON was first introduced in 2001 as an alternative to the more complex XML format. It quickly gained popularity due to its lightweight and easy-to-read syntax. JSON has become the standard format for data transmission in web development and is widely supported by most languages.

One alternative to working with JSON in PHP is to use the `serialize()` and `unserialize()` functions, which serialize data into a string and unserialize it back into a PHP object. However, this approach is less flexible and not as widely used as JSON.

When working with JSON in PHP, it's important to ensure that the data is properly encoded and decoded to prevent any errors. The `json_last_error()` function can be useful for troubleshooting and handling potential errors when working with JSON.

## See Also:
To learn more about working with JSON in PHP, check out the official PHP documentation on [JSON functions](https://www.php.net/manual/en/ref.json.php). You can also read up on [best practices](https://www.json.org/json-en.html) for working with JSON and how to handle potential security risks.