---
title:                "Working with YAML"
aliases:
- /en/javascript/working-with-yaml/
date:                  2024-02-03T19:03:12.945585-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

YAML, short for YAML Ain't Markup Language, is a human-readable data serialization format. Programmers often use it for configuration files and data exchange between languages due to its simplicity and readability compared to JSON or XML.

## How to:

In JavaScript, working with YAML typically involves using a third-party library since the language doesn't include a built-in parser for YAML. One of the most popular libraries for this purpose is `js-yaml`. You can use `js-yaml` to parse YAML into JavaScript objects and vice versa. 

First, you need to install `js-yaml`:

```bash
npm install js-yaml
```

Then, you can use it in your projects. Here’s how you can load a YAML file and parse it into a JavaScript object:

```javascript
// Require the js-yaml module
const yaml = require('js-yaml');
const fs   = require('fs');

// Load YAML from a file
try {
  const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

If your `config.yaml` file looks like this:

```yaml
version: 1
services:
  web:
    image: "myapp/web:latest"
    ports:
      - "5000:5000"
```

The output will be:

```javascript
{ version: 1,
  services: 
   { web: 
      { image: 'myapp/web:latest',
        ports: [ '5000:5000' ] } } }
```

To do the reverse, converting a JavaScript object to a YAML string:

```javascript
const yaml = require('js-yaml');
const obj = {
  version: 1,
  services: {
    web: {
      image: "myapp/web:latest",
      ports: ["5000:5000"]
    }
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

This code will produce:

```yaml
version: 1
services:
  web:
    image: myapp/web:latest
    ports:
      - '5000:5000'
```

Using `js-yaml`, you can easily integrate YAML parsing and serialization into your JavaScript projects, enhancing data interexchangeability and configuration management.
