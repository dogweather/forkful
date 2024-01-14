---
title:                "TypeScript recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why
YAML is a popular choice for structuring configuration files due to its human-readable syntax and flexibility. It is commonly used for application configuration, deployment, and data serialization. In this blog post, we will explore how to work with YAML in a TypeScript project.

## How To
To start using YAML in TypeScript, we first need to install the YAML library. This can be done by running the following command in your terminal:

```TypeScript
npm install yaml
```

Once the library is installed, we can begin importing it into our code. The code snippet below shows an example of reading a YAML file and accessing its properties:

```TypeScript
import * as yaml from 'yaml';

const configFile = fs.readFileSync('config.yaml', 'utf8');
const config = yaml.parse(configFile);

console.log(config.server.host); // Output: localhost
console.log(config.server.port); // Output: 3000
```

In the example above, we use the `yaml` library to parse a YAML file and store its contents in the `config` variable. We can then access the properties of `config` using dot notation, just like we would with a JavaScript object.

We can also use YAML to create and write to a YAML file. The code snippet below shows how to do this:

```TypeScript
import * as yaml from 'yaml';

const config = {
  user: {
    name: 'John Doe',
    age: 25,
    hobbies: ['coding', 'reading', 'gaming']
  }
};

const yamlString = yaml.stringify(config);
fs.writeFileSync('user.yaml', yamlString);
```

The above code creates a `config` variable with a user object that contains the user's information. We then use the `yaml` library to convert the `config` object into a YAML string and write it to a file named "user.yaml". The resulting file will look like this:

```yaml
user:
  name: John Doe
  age: 25
  hobbies:
    - coding
    - reading
    - gaming
```

## Deep Dive
YAML, which stands for "YAML Ain't Markup Language", is a human-readable data serialization format. It is often used for application configuration, but can also be used for data exchange. YAML is designed to be easy to read and write for both humans and machines.

Some key features of YAML include:

- Structured data: YAML allows for data to be organized in a structured format, using indentation to denote relational elements.
- Support for various datatypes: YAML supports common data types like strings, numbers, arrays, and objects, making it flexible for different use cases.
- Comments: YAML allows for comments to be added, which can be helpful for providing context or explanations within configuration files.
- Anchors: YAML supports the usage of anchors and aliases, allowing for the reuse of data within the same file.

For more information on working with YAML, check out the official YAML specification and the `yaml` library documentation.

## See Also
- [Official YAML Spec](https://yaml.org/)
- [yaml library documentation](https://eemeli.org/yaml/)