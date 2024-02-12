---
title:                "Working with YAML"
date:                  2024-02-03T19:03:21.023196-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

YAML, which stands for "YAML Ain't Markup Language", is a human-readable data serialization format that is commonly used for configuration files. Programmers opt to utilize YAML due to its simplicity and readability, making it an excellent choice for storing settings, parameters, and even complex data structures in an easily manageable form.

## How to:

PHP, as of its current iterations, does not support parsing YAML as part of its standard library. The most straightforward way to work with YAML in PHP is by using the Symfony YAML component or the `yaml` PECL extension.

### Using Symfony YAML Component

First, install the Symfony YAML component via Composer:

```bash
composer require symfony/yaml
```

Then, you can parse and dump YAML content as follows:

```php
<?php
require_once __DIR__.'/vendor/autoload.php';

use Symfony\Component\Yaml\Yaml;

// Parsing YAML
$yamlString = <<<YAML
greet: Hello, World!
framework:
  name: Symfony
  language: PHP
YAML;

$array = Yaml::parse($yamlString);
print_r($array);

// Creating YAML from an array
$array = [
    'greet' => 'Hello, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = Yaml::dump($array);
echo $yaml;
```

Sample output when parsing:

```
Array
(
    [greet] => Hello, World!
    [framework] => Array
        (
            [name] => Symfony
            [language] => PHP
        )

)
```

Sample output when dumping:

```
greet: Hello, YAML!
framework:
    name: Symfony
    language: PHP
```

### Using `yaml` PECL Extension

If you prefer, or if your project requirements allow, the PECL extension can be another efficient way to work with YAML. First, ensure the extension is installed:

```bash
pecl install yaml
```

Then, enable it in your `php.ini` configuration:

```ini
extension=yaml.so
```

Here's how to parse and emit YAML:

```php
<?php

// Parsing YAML
$yamlString = <<<YAML
greet: Hello, World!
framework:
  name: Symfony
  language: PHP
YAML;

$array = yaml_parse($yamlString);
print_r($array);

// Creating YAML from an array
$array = [
    'greet' => 'Hello, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = yaml_emit($array);
echo $yaml;
```

The output will be similar to the Symfony component's, illustrating YAML's role as a bridge between human-readable format and PHP array structures, facilitating easier configuration and data handling.
