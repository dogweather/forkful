---
title:                "Working with yaml"
html_title:           "PHP recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

Working with YAML in PHP allows programmers to easily store and retrieve data in a human-readable format. It is a popular choice for configuration files, as well as for exchanging data between different systems. It offers a more compact and structured alternative to languages like XML or JSON.

## How to:

To get started with working with YAML in PHP, you will need to install the YAML extension. You can do this by running the following command in your terminal:

```PHP
pecl install yaml
```

Next, you will need to enable the extension by adding the following line to your php.ini file:

```PHP
extension=yaml.so
```

Once the extension is installed and enabled, you can start using it in your PHP code. The ```yaml_parse()``` function can be used to convert a YAML string into a PHP array, and ```yaml_emit()``` can be used to convert a PHP array into a YAML string. Here is an example of how to use these functions:

```PHP
// Create a YAML string
$yaml_string = "
name: John
age: 25
hobbies:
  - coding
  - hiking
";

// Convert YAML string to PHP array
$php_array = yaml_parse($yaml_string);

// Convert PHP array back to YAML string
$new_yaml_string = yaml_emit($php_array);

// Output new YAML string
var_dump($new_yaml_string);
```

The output will be:

```
string(47) "name: John
age: 25
hobbies:
    - coding
    - hiking"
```

You can also use the ```dump()``` function to output the PHP array in a more human-readable format. Here is an example:

```PHP
// Output PHP array
dump($php_array);
```

The output will be:

```
array:3 [▼
  "name" => "John"
  "age" => 25
  "hobbies" => array:2 [▼
    0 => "coding"
    1 => "hiking"
  ]
]
```

## Deep Dive:

YAML stands for "YAML Ain't Markup Language" and was first released in 2001. It was originally designed as a human-friendly way to create configuration files that are easy to read and update. The syntax is simple and similar to writing lists or dictionaries in other languages.

Alternative options for storing and exchanging data in PHP include XML and JSON. XML is often used for more complex data structures, but its syntax can be cumbersome. JSON is also a popular choice, but it has stricter formatting requirements and can be challenging to maintain for large data sets.

Under the hood, the YAML extension in PHP uses the libyaml library, which has C functions for parsing and emitting YAML strings. This makes it a faster and more efficient option compared to some PHP-based YAML libraries.

## See Also:

- [Official Documentation for YAML in PHP](https://www.php.net/manual/en/book.yaml.php)
- [YAML Specification](https://yaml.org/spec/1.2/spec.html)