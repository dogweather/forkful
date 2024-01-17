---
title:                "Working with yaml"
html_title:           "C recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

Working with YAML is a way for programmers to store and share structured data in a clear and human-readable format. It is especially useful for configuration files and data serialization, making it easier for non-technical team members to understand and modify. Many popular applications, such as Ansible and Kubernetes, use YAML as their configuration language.

## How to:

To start working with YAML in your C program, you'll need to include the libyaml library in your project. Then, you can use functions like `yaml_parser_parse()` and `yaml_emitter_emit()` to parse and emit YAML data.

```C
#include <yaml.h>

// Create a YAML emitter and emit a basic YAML document
yaml_emitter_t emitter;
yaml_emitter_initialize(&emitter);
yaml_stream_start_event_initialize(&event, YAML_UTF8_ENCODING);
yaml_emitter_emit(&emitter, &event);

// Parse a YAML document and access its data
yaml_parser_t parser;
yaml_parser_initialize(&parser);
yaml_parser_set_input_file(&parser, "config.yaml");
while (!done) {
    yaml_event_t event;
    if (!yaml_parser_parse(&parser, &event)) break;
    // handle different event types: YAML_SCALAR_EVENT, YAML_SEQUENCE_START_EVENT, etc.
}
```

## Deep Dive

YAML, which stands for "YAML Ain't Markup Language," was created by Ingy döt Net and Clark Evans in 2001 in response to the complexity of XML and the limitations of JSON. It is a human-readable data serialization language and was designed to be easily readable and editable by humans without specialized software.

There are alternative data serialization formats, such as JSON and XML, but YAML sets itself apart with its readability and flexibility. YAML allows for multiple data types, including strings, numbers, and booleans, whereas JSON is limited to only strings, numbers, and arrays. Additionally, YAML allows for comments and anchors/aliases, making it ideal for complex and hierarchical data structures.

Implementation-wise, YAML is a straightforward language to work with. The libyaml library, written in C, is the official parser and emitter for YAML and is actively maintained by a community of developers. It is also lightweight and does not require any external dependencies.

## See Also
- [libyaml official docs](https://pyyaml.org/wiki/LibYAML)
- [YAML Specification](https://yaml.org/spec/1.2/spec.html)
- [YAML Ain't Markup Language (YAML™) Version 1.2](https://yaml.org/spec/1.2/spec.html)