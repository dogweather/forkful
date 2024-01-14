---
title:                "PHP recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why
Why would anyone want to work with YAML in their PHP programming? Well, YAML (YAML Ain't Markup Language) is a popular data serialization language that is easy to read and write for humans. It is commonly used for configuration files, making it a great tool for web development and server management.

## How To
To start working with YAML in PHP, you'll need to install the Symfony YAML component using Composer. Once installed, you can use the `Yaml::parse()` function to convert YAML data into a PHP array. Here's an example:

```PHP
use Symfony\Component\Yaml\Yaml;

// Read YAML file
$yamlContent = file_get_contents('my_config.yaml');

// Convert to PHP array
$config = Yaml::parse($yamlContent);

// Output value of "database" key
echo $config['database'];
```

If your YAML data contains multiple documents, you can use the `parseMultiple()` function to parse them all at once and return an array of arrays. Additionally, you can use the `Yaml::dump()` function to convert a PHP array into YAML format. This is useful when writing configuration files or saving data to a YAML file.

```PHP
use Symfony\Component\Yaml\Yaml;

// Define an array with data
$data = ['name' => 'John', 'age' => 25, 'hobbies' => ['running', 'cooking', 'reading']];

// Convert to YAML format
$yaml = Yaml::dump($data);

// Output YAML string
echo $yaml;
```

**Output:**

```
name: John
age: 25
hobbies:
  - running
  - cooking
  - reading
```

## Deep Dive
If you want to dive deeper into working with YAML in PHP, there are several additional features and options available. For example, the Symfony YAML component allows you to set a custom indentation and line break for your YAML output. You can also use the `parseFile()` function to directly parse a YAML file instead of first retrieving its contents.

When working with more complex YAML data, you may run into issues with type conversions. In these cases, you can use the `Yaml::parse()` function with the `PARSE_OBJECT` option to ensure that numeric and boolean values are converted correctly.

## See Also
- [Symfony YAML Component Documentation](https://symfony.com/doc/current/components/yaml.html)
- [YAML Specification](https://yaml.org/spec/)