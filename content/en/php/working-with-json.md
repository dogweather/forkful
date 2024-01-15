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

## Why

JSON (JavaScript Object Notation) is a popular data format used to transfer and store information in a structured way. With the rise of web development and API-based applications, knowledge of working with JSON has become essential for developers in order to effectively communicate and transfer data. It is also supported by a wide range of programming languages, making it a versatile choice for data manipulation.

## How To

To work with JSON in PHP, we can use a few built-in functions: `json_encode()` and `json_decode()`. These functions allow us to convert PHP arrays into a JSON string, and vice versa.

### Encoding Data to JSON

```PHP
// Sample PHP array
$data = [
    'name' => 'John Doe',
    'age' => 25,
    'occupation' => 'Web Developer'
];

// Convert to JSON string
$json = json_encode($data);

// Output: {"name": "John Doe", "age": 25, "occupation": "Web Developer"}
echo $json;
```

### Decoding JSON to PHP

```PHP
// Sample JSON string
$json = '{"name": "John Doe", "age": 25, "occupation": "Web Developer"}';

// Convert to PHP array
$data = json_decode($json, true);

// Output: Array ( [name] => John Doe, [age] => 25, [occupation] => Web Developer )
print_r($data);
```

## Deep Dive

Apart from basic encoding and decoding, there are various other functions in PHP that allow us to work with JSON data. These include `json_last_error()` to get the last occurred error when working with JSON, `JSON_UNESCAPED_UNICODE` flag to encode unicode characters without escaping, and `JSON_PRETTY_PRINT` flag to format the output with indentation for better readability.

It is also important to note that JSON supports various data types such as strings, numbers, objects, and arrays, making it a versatile choice for data storage and manipulation. However, it does not support functions or undefined values.

## See Also

- [PHP Official Documentation on JSON](https://www.php.net/manual/en/book.json.php)
- [JSON.org](https://www.json.org/json-en.html)
- [How to Use JSON with PHP - Tutorial by W3Schools](https://www.w3schools.com/js/js_json_php.asp)