---
title:                "Working with YAML"
date:                  2024-01-19
html_title:           "Arduino recipe: Working with YAML"
simple_title:         "Working with YAML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

YAML is a human-readable data serialization format used for config files, data exchange between languages, and data storage. Programmers choose YAML for its simplicity and readability, making it a breeze to use for quick configuration and development tasks.

## How to:

C doesn't have built-in YAML parsing, so we use a library like `libyaml` to handle YAML files. Here's a simple example of parsing a YAML file in C.

First, include the library:
```C
#include <yaml.h>
```

Next, initialize a parser, open a file, and start parsing:
```C
FILE *fh = fopen("config.yaml", "r");
yaml_parser_t parser;
yaml_parser_initialize(&parser);
yaml_parser_set_input_file(&parser, fh);

yaml_event_t event;
/* Read the event sequence */
while (true) {
    if (!yaml_parser_parse(&parser, &event)) {
        printf("Parser error %d\n", parser.error);
        exit(EXIT_FAILURE);
    }

    if (event.type == YAML_SCALAR_EVENT) {
        printf("Got scalar (value): %s\n", event.data.scalar.value);
    }

    if (event.type == YAML_STREAM_END_EVENT) {
        break;
    }

    yaml_event_delete(&event);
}

/* Cleanup */
yaml_parser_delete(&parser);
fclose(fh);
```

Sample `config.yaml` content:
```yaml
name: John Doe
age: 30
```

Sample output:
```
Got scalar (value): name
Got scalar (value): John Doe
Got scalar (value): age
Got scalar (value): 30
```

## Deep Dive

YAML stands for "YAML Ain't Markup Language." It emerged in the early 2000s as an alternative to XML for configuration files, aiming for human readability. YAML is used in many tools (like Docker, Kubernetes, etc.) and is often favored over JSON for configs due to its support for comments and cleaner syntax.

Common C alternatives for working with YAML are `libyaml` and `yaml-cpp` (though the latter is for C++). These libraries allow C/C++ programs to serialize and deserialize YAML data.

When parsing YAML, your program builds a tree in memory. Nodes in this tree can be mappings (like dictionaries or hash tables), sequences (like arrays), or scalars (strings, numbers, etc.). libyaml's parser is event-driven, meaning it reads the YAML stream and emits events for each YAML structure encountered. Handling these events lets you construct or operate on the corresponding data structure.

## See Also

- `libyaml` GitHub: https://github.com/yaml/libyaml
- YAML official specs: https://yaml.org/spec/1.2/spec.html
- "Programming with libyaml" tutorial: https://libyaml.docsforge.com/master/programming-with-libyaml/
- Comparison of data serialization formats: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
