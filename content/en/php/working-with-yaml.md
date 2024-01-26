---
title:                "Working with YAML"
html_title:           "Arduino recipe: Working with YAML"
simple_title:         "Working with YAML"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

YAML is a human-friendly data serialization format. Programmers use it for configuration files, data exchange, and storage due to its readability and simplicity.

## How to:

To play with YAML in PHP, you need the `yaml` extension. Here's a quick spin-up:

**Install YAML extension** (if not installed):
```bash
pecl install yaml
```

**Load Extension**:
Ensure your `php.ini` includes:
```ini
extension=yaml
```

**Parsing YAML**: 
```php
<?php
$yamlString = "
settings:
  database: MySQL
  host: localhost";

$array = yaml_parse($yamlString);

print_r($array);
```
**Sample Output**:
```
Array
(
    [settings] => Array
        (
            [database] => MySQL
            [host] => localhost
        )
)
```

**Creating YAML**:
```php
<?php
$array = [
  'settings' => [
    'database' => 'MySQL',
    'host' => 'localhost'
  ]
];

$yamlString = yaml_emit($array);
echo $yamlString;
```
**Sample Output**:
```
settings:
  database: MySQL
  host: localhost
```

## Deep Dive:

YAML, which stands for "YAML Ain't Markup Language," focuses on data and data structures, and it excels where languages like XML might be overly complex. It was first released in 2001. Alternatives include JSON and XML; YAML is often preferred for its human readability. PHP's `yaml` extension hooks into the `libyaml` library, ensuring fast parsing and emitting.

## See Also:

- PHP Official YAML Extension Docs: https://www.php.net/manual/en/book.yaml.php
- YAML Official Site: https://yaml.org
- Comparison of data serialization formats: https://en.wikipedia.org/wiki/Comparison_of_data_serialization_formats
