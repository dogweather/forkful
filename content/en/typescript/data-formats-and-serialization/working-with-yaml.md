---
title:                "Working with YAML"
aliases:
- /en/typescript/working-with-yaml.md
date:                  2024-02-03T19:03:32.739842-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML, a data serialization language designed to be human-friendly, is often used for configuration files, inter-process messaging, and data storage. Programmers lean on YAML for its readability and ease of use, especially when dealing with complex structured data, making it an excellent choice for applications developed in TypeScript.

## How to:
Working with YAML in TypeScript typically involves parsing YAML content into JavaScript objects and possibly converting JavaScript objects back into YAML. This requires a parser; one popular choice is `js-yaml`, a library that can easily be integrated into TypeScript projects.

### Installing js-yaml
First, add `js-yaml` to your project:

```bash
npm install js-yaml
```

### Parsing YAML to JavaScript Object
Imagine you have a YAML file `config.yaml` with the following content:

```yaml
database:
  host: localhost
  port: 5432
  username: user
  password: pass
```

You can read and parse this file into a JavaScript object as follows:

```typescript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

// Load and parse the YAML file
const fileContents = fs.readFileSync('./config.yaml', 'utf8');
const data = yaml.load(fileContents) as Record<string, any>;

console.log(data);
```

**Sample Output:**

```json
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "username": "user",
    "password": "pass"
  }
}
```

### Converting JavaScript Object to YAML
If you need to go the other way and convert a JavaScript object to a YAML string, you can use `js-yaml` as follows:

```typescript
import * as yaml from 'js-yaml';

const obj = {
  title: "Example",
  is_published: true,
  author: {
    name: "Jane Doe",
    age: 34
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

**Sample Output:**

```yaml
title: Example
is_published: true
author:
  name: Jane Doe
  age: 34
```

This snippet converts a JavaScript object to a YAML string and outputs it. In practice, you might write this back to a file or use it in other parts of your application.
