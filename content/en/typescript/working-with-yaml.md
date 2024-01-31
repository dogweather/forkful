---
title:                "Working with YAML"
date:                  2024-01-19
html_title:           "Arduino recipe: Working with YAML"
simple_title:         "Working with YAML"

category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

YAML is a human-friendly data serialization standard. Programmers use it for config files, data exchange between languages, and more because it's simple and readable.

## How to:

To work with YAML in TypeScript, you'll need a library like `js-yaml`. Install it first:

```bash
npm install js-yaml
```

Now, parse a YAML string to a JavaScript object:

```typescript
import yaml from 'js-yaml';

const yamlStr = `
name: John Doe
age: 30
`;

try {
  const doc = yaml.load(yamlStr);
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

Sample output:

```json
{ name: 'John Doe', age: 30 }
```

To convert an object to a YAML string:

```typescript
import yaml from 'js-yaml';

const obj = { name: 'Jane Doe', age: 25 };

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

Sample output:

```yaml
name: Jane Doe
age: 25
```

## Deep Dive

YAML started in 2001, aiming for human readability and data interchange between languages. It's a superset of JSON. Alternatives include JSON and XML, but YAML's minimal syntax is often preferred for configuration files. As you work with YAML in TypeScript, remember that it's untyped; careful with the data received, especially from untrusted sources, to avoid security issues.

## See Also

- Official YAML website: http://yaml.org
- `js-yaml` GitHub repo: https://github.com/nodeca/js-yaml
- YAML vs. JSON comparison: https://en.wikipedia.org/wiki/YAML#Comparison_with_JSON
