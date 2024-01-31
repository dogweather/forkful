---
title:                "Working with YAML"
date:                  2024-01-19
html_title:           "Arduino recipe: Working with YAML"
simple_title:         "Working with YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML ain't Markup Language, or YAML, is a human-friendly data serialization standard for all programming languages. Programmers work with YAML because it's easy to read and write, often used for config files, and data exchange between languages or services.

## How to:
We'll use the popular `js-yaml` library to parse YAML into JavaScript objects and stringify JavaScript objects into YAML.

1. First, install the library:

```bash
npm install js-yaml
```

2. Parse YAML to JavaScript:

```javascript
const yaml = require('js-yaml');
const fs = require('fs');

try {
  const doc = yaml.load(fs.readFileSync('config.yml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

Sample output if `config.yml` is:

```yaml
version: 1
services:
  - webapp
  - database
```

Might look like:

```javascript
{ version: 1, services: [ 'webapp', 'database' ] }
```

3. Stringify JavaScript to YAML:

```javascript
const yaml = require('js-yaml');
const fs = require('fs');

let data = {
  title: "YAML Example",
  description: "YAML is easy"
};

try {
  const ymlText = yaml.dump(data);
  fs.writeFileSync('example.yml', ymlText, 'utf8');
} catch (e) {
  console.error(e);
}
```

This will create a file `example.yml` with:

```yaml
title: YAML Example
description: 'YAML is easy'
```

## Deep Dive
YAML started in 2001, designed to be easily readable by humans and comfortably writable by hand. JSON and XML are alternatives but aren't as straightforward for humans. YAML's simplicity can lead to security issues if not correctly implemented, like keeping `!!python/object/apply` disabled to prevent arbitrary code execution. Libraries like `js-yaml` provide options to customize the parsing and stringifying of YAML to add security and functionality.

## See Also
- YAML 1.2 spec: https://yaml.org/spec/1.2/spec.html
- js-yaml GitHub repo: https://github.com/nodeca/js-yaml
- YAML Wikipedia entry for more background information: https://en.wikipedia.org/wiki/YAML
- JSON vs YAML comparison: https://phoenixnap.com/kb/yaml-vs-json
