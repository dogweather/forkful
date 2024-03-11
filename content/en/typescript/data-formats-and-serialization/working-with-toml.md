---
date: 2024-01-25 03:40:11.764132-07:00
description: "TOML, short for Tom's Obvious, Minimal Language, is a data serialization\
  \ format akin to JSON or YAML. Programmers use it for its human readability and\u2026"
lastmod: '2024-03-11T00:14:33.739266-06:00'
model: gpt-4-1106-preview
summary: "TOML, short for Tom's Obvious, Minimal Language, is a data serialization\
  \ format akin to JSON or YAML. Programmers use it for its human readability and\u2026"
title: Working with TOML
---

{{< edit_this_page >}}

## What & Why?
TOML, short for Tom's Obvious, Minimal Language, is a data serialization format akin to JSON or YAML. Programmers use it for its human readability and straightforward mapping to data types, making it a go-to for config files and data interchange.

## How to:
First, you'll need a TOML parser. `@iarna/toml` is a popular choice. Install it with npm: `npm install @iarna/toml --save`. Here's how you read a TOML file and parse it to a JavaScript object:

```typescript
import * as fs from 'fs';
import toml from '@iarna/toml';

const tomlContent = fs.readFileSync('config.toml', 'utf-8');
const parsedData = toml.parse(tomlContent);

console.log(parsedData);
```
If `config.toml` contains:
```
[server]
port = 8080
```
The output would be:
```
{ server: { port: 8080 } }
```
And, writing to a TOML file is just as straightforward:
```typescript
import * as fs from 'fs';
import { stringify } from '@iarna/toml';

const obj = { server: { port: 8080 } };
const tomlString = stringify(obj);
fs.writeFileSync('config.toml', tomlString);
``` 
Running this code writes the object to `config.toml` in TOML format.

## Deep Dive
TOML was created by Tom Preston-Werner, the co-founder of GitHub, around 2013 as a response to the limitations he perceived in other formats like INI or YAML. It's designed to be unambiguous and easy to parse into data structures, hence, a favorite for configuration files. Alternatives like JSON lack comments, while YAML is more complex. TOML shines in its simplicity and its ability to clearly represent complex data hierarchies.

Under the hood, when you parse TOML in TypeScript, you're converting textual data into a structured format the language can manipulate. This involves lexing (turning raw text into tokens) and parsing (building an internal data structure); `@iarna/toml` handles both seamlessly. The emoji support is a fun touch, showing TOML's user-centric approach.

## See Also
- TOML Official Spec: https://toml.io/en/
- `@iarna/toml` package: https://www.npmjs.com/package/@iarna/toml
- Comparisons between TOML, YAML, and JSON: https://blog.bitsrc.io/choosing-the-right-configuration-file-format-toml-vs-yaml-vs-json-71b5be8968ea
