---
title:                "Working with JSON"
html_title:           "Arduino recipe: Working with JSON"
simple_title:         "Working with JSON"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) is a lightweight data format for data interchange. Programmers use it because it's easy to read/write and language-independent, making it ideal for APIs and web services.

## How to:

### Encode an array to JSON
```php
$array = ['foo' => 'bar', 'baz' => 'qux'];
$json = json_encode($array);
echo $json; // {"foo":"bar","baz":"qux"}
```

### Decode JSON into an object
```php
$json = '{"foo":"bar","baz":"qux"}';
$object = json_decode($json);
echo $object->foo; // bar
```

### Decode JSON into an associative array
```php
$json = '{"foo":"bar","baz":"qux"}';
$array = json_decode($json, true);
echo $array['foo']; // bar
```

### Handle JSON errors
```php
$json = '{"foo":"bar,"baz":"qux"}'; // Note the missing quote
$array = json_decode($json, true);

if(json_last_error() != JSON_ERROR_NONE) {
   echo json_last_error_msg(); // Syntax error, malformed JSON
}
```

## Deep Dive

JSON has been the de facto standard for web data interchange since the early 2000s, replacing XML due to its simplicity. Alternatives like XML and YAML exist, but JSON's compactness and speed have made it a top choice. The PHP `json_encode()` and `json_decode()` functions serialize and unserialize data, respectively. Since PHP 5.4.0, the `JSON_PRETTY_PRINT` option makes output more readable, and as of PHP 7.3.0, developers can throw `JsonException` for error handling, making JSON parsing more robust.

## See Also

- PHP Manual on JSON: https://www.php.net/manual/en/book.json.php
- JSON Homepage: http://json.org/
- PHP The Right Way (JSON handling section): https://phptherightway.com/#json
- Composer, a dependency manager for PHP (uses JSON for package info): https://getcomposer.org/