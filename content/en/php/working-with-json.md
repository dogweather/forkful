---
title:                "PHP recipe: Working with json"
simple_title:         "Working with json"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/working-with-json.md"
---

{{< edit_this_page >}}

## Why
JSON (JavaScript Object Notation) has become one of the most popular data interchange formats used in web development. It is a lightweight and easy-to-read format that allows developers to efficiently store and transmit data between client and server applications. Working with JSON is an essential skill for any PHP programmer, as it is widely used in APIs, databases, and other web services.

## How To
Using JSON in PHP is very straightforward and requires only a few lines of code to get started. Let's take a look at a simple example:

```PHP
// Define an associative array with some data
$data = [
    'name' => 'John Smith',
    'age' => 35,
    'hobbies' => ['reading', 'gaming', 'hiking']
];

// Convert the array to JSON
$json = json_encode($data);

// Output the JSON string
echo $json;
```
The above code will encode the data array into a JSON string and display it on the screen as follows:
```
{"name":"John Smith","age":35,"hobbies":["reading","gaming","hiking"]}
```

To decode a JSON string back into a PHP object or array, we can use the `json_decode()` function. Let's see an example:

```PHP
// A JSON string
$json = '{"name":"John Smith","age":35,"hobbies":["reading","gaming","hiking"]}';

// Decode the JSON into an object
$object = json_decode($json);

// Output the name property
echo $object->name;

// Decode the JSON into an array
$array = json_decode($json, true);

// Output the first hobby
echo $array['hobbies'][0];
```

In addition to encoding and decoding, PHP also provides other helpful functions for working with JSON, such as `json_last_error()` to check for any errors during encoding or decoding, and `json_last_error_msg()` to get a human-readable error message.

## Deep Dive
Although JSON appears to be a simple key-value paired format, it can actually hold more complex data structures such as nested objects and arrays. It also supports data types such as strings, numbers, booleans, and null values. This makes it a versatile format for storing and transmitting data.

There are also some helpful functions that allow us to manipulate JSON data, such as `json_encode()` with options to format the data for better readability. We can also use `json_decode()` with the `FILTER_VALIDATE_BOOLEAN` filter to convert JSON booleans to PHP booleans automatically.

Another thing worth mentioning is that PHP has extensions like `jsond` and `cgmagick` which provides advanced features and performance optimizations for working with JSON. So, if you need to handle large amounts of JSON data, it's worth checking out these extensions.

## See Also
* [PHP Manual: JSON](https://www.php.net/manual/en/book.json.php)
* [Working with JSON in PHP: A Beginner's Guide](https://www.freecodecamp.org/news/how-to-work-with-json-in-php/)
* [The Power of JSON in PHP](https://www.sitepoint.com/power-json-php/)
* [PHP JSON functions with extensions](https://www.php.net/manual/en/ref.json.php)