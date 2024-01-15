---
title:                "Working with yaml"
html_title:           "Javascript recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why
YAML (YAML Ain't Markup Language) is a popular human-readable data serialization language that is commonly used for configuration files. It offers a simpler and more readable alternative to formats like XML and JSON, making it a popular choice among developers.

## How To
Using YAML in Javascript is made easy with the help of libraries like js-yaml and yaml-js. Here's a quick example of how to load and parse a YAML file using js-yaml:

```Javascript
const yaml = require('js-yaml');
const fs = require('fs');

// Load the YAML file
let data = fs.readFileSync('config.yml', 'utf8');

// Parse the data
let config = yaml.safeLoad(data);

// Access the data
console.log(config.title); // Output: "My Website"
console.log(config.author); // Output: "John Doe"
```

And here's how the config.yml file would look like:

```YAML
title: My Website
author: John Doe
```

Using yaml-js, the process would look slightly different:

```Javascript
const YAML = require('yaml-js');
const fs = require('fs');

// Load the YAML file
let data = fs.readFileSync('config.yml', 'utf8');

// Parse the data
let config = YAML.load(data);

// Access the data
console.log(config.title); // Output: "My Website"
console.log(config.author); // Output: "John Doe"
```

Both libraries offer similar functionality with some minor differences in syntax.

## Deep Dive
YAML supports a wide range of data types, including strings, numbers, booleans, arrays, and objects. It also allows for indentation-based nesting, making it easy to create structured and hierarchical data.

One of the key advantages of YAML is its readability. It uses human-friendly syntax that is easy to understand, making it a great choice for configuration files that need to be frequently updated and maintained.

Additionally, YAML supports comments, which can help provide context or explanations for certain sections of the data. This can be especially useful when working with large or complex files.

It's worth noting that YAML is not without its limitations. While it supports most basic data types, it does not support functions or any other executable code. This means it should not be used to store sensitive or critical data.

## See Also
- YAML Official Website: https://yaml.org/
- js-yaml library: https://github.com/nodeca/js-yaml
- yaml-js library: https://github.com/tj/js-yaml-js