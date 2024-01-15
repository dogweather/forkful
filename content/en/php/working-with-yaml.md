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

## Why

YAML, short for "YAML Ain't Markup Language", is a popular language used for data serialization. It offers a human-readable and easy-to-use syntax, making it a preferred choice for storing and transferring data in various applications. In the world of PHP programming, understanding how to work with YAML can be incredibly useful and beneficial.

## How To

First, we need to make sure that the YAML extension is enabled in our PHP configuration. To do so, we can check the "Loaded Configuration File" section in phpinfo() or use the command "php -i | grep yaml". If the extension is not enabled, we can follow these steps to enable it:

```PHP
// Install YAML extension on Ubuntu
sudo apt-get install php-yaml

// Install YAML extension on CentOS
sudo yum install php-pecl-yaml

// Enable YAML extension manually
vi /path/to/php.ini
// Add the following line at the end
extension=yaml.so 
```

Once the extension is enabled, we can start working with YAML in our PHP code. Let's take a look at some coding examples:

### Writing YAML

```PHP
<?php 
$myArray = array(
   'name' => 'John',
   'age' => 25,
   'hobbies' => array(
       'reading',
       'hiking',
       'painting'
   )
);

// Convert array to YAML string
$yamlStr = yaml_emit($myArray);
```

The output of this will be:

```
name: John
age: 25
hobbies:
    - reading
    - hiking
    - painting
```

### Reading YAML

```PHP
<?php 
// YAML string 
$yamlStr = "
name: John
age: 25
hobbies:
    - reading
    - hiking
    - painting
";
// Convert YAML string to array
$myArray = yaml_parse($yamlStr);
// Access specific values using array keys
echo "Name: " . $myArray['name']; // Output: Name: John
echo "Hobby 1: " . $myArray['hobbies'][0]; // Output: Hobby 1: reading 
```

## Deep Dive

YAML offers a wide range of features such as support for complex data types, comments, and references. It also has the ability to merge multiple YAML documents into one, making it a powerful tool for handling large and complex data structures.

One of the most useful features of YAML is the support for anchors and aliases. This allows us to define a value once and refer to it in different places in the document. This is especially helpful when dealing with nested data structures.

### Anchors and Aliases Example

```PHP
<?php 
$yamlStr = "
book1:
    title: Wings of Fire
    author: Tui T. Sutherland
    genre: fantasy
book2:
    <<: *book1
    title: The Lost Heir
";

// Convert YAML string to array
$books = yaml_parse($yamlStr);

// Access specific values using array keys
echo "Book 1 title: " . $books['book1']['title']; // Output: Book 1 title: Wings of Fire
echo "Book 2 title: " . $books['book2']['title']; // Output: Book 2 title: The Lost Heir (inherits values from book1)
```

## See Also

- Official YAML website: https://yaml.org/
- PHP documentation for YAML: https://www.php.net/manual/en/book.yaml.php
- A beginner's guide to YAML: https://www.digitalocean.com/community/tutorials/an-introduction-to-yaml