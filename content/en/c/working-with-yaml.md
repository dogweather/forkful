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

## Why

So, you're interested in learning about YAML in C? Well, YAML (YAML Ain't Markup Language) is a human-readable data serialization format that is widely used for configuration files and storing structured data. It is a lightweight and flexible format, making it popular among developers.

## How To

To start using YAML in your C code, you will need a library that can parse and manipulate YAML documents. One popular library is LibYAML, which is a lightweight and fast C library for working with YAML.

Here's an example of how to use LibYAML to load a YAML file and access its data:

```C
#include <yaml.h>
#include <stdlib.h>

int main(void) {
    // Open the YAML file
    FILE *file = fopen("sample.yaml", "rb");

    // Initialize the parser
    yaml_parser_t parser;
    if (!yaml_parser_initialize(&parser)) {
        fputs("Failed to initialize parser!\n", stderr);
        return EXIT_FAILURE;
    }

    // Set the input file
    yaml_parser_set_input_file(&parser, file);

    // Parse the file
    yaml_event_t event;
    do {
        if (!yaml_parser_parse(&parser, &event)) {
            fputs("Parser error %d\n", stderr);
            return EXIT_FAILURE;
        }

        // Access the data
        if (event.type == YAML_SCALAR_EVENT) {
            printf("Data: %s\n", event.data.scalar.value);
        }
    } while (event.type != YAML_STREAM_END_EVENT);

    // Cleanup
    yaml_parser_delete(&parser);
    fclose(file);

    return EXIT_SUCCESS;
}
```

Sample output for a YAML file containing the scalar value `Hello World`:

```
Data: Hello World
```

## Deep Dive

LibYAML also allows you to create and modify YAML documents in your C code. Here's an example of how to create a YAML document and write it to a file:

```C
#include <yaml.h>
#include <stdlib.h>

int main(void) {
    // Create a new YAML document
    yaml_document_t document;
    yaml_document_initialize(&document, NULL, NULL, NULL, 0, 0);

    // Create a sequence
    yaml_node_t *node = yaml_document_add_sequence(&document, NULL, YAML_FLOW_SEQUENCE_STYLE);

    // Add scalar values to the sequence
    yaml_node_t *item = yaml_document_add_scalar(&document, NULL, "Apple");
    yaml_document_append_sequence_item(&document, node, item);
    item = yaml_document_add_scalar(&document, NULL, "Orange");
    yaml_document_append_sequence_item(&document, node, item);
    item = yaml_document_add_scalar(&document, NULL, "Banana");
    yaml_document_append_sequence_item(&document, node, item);

    // Write the document to a file
    FILE *file = fopen("fruits.yaml", "wb");
    yaml_document_dump(&document, file);
    fclose(file);

    // Cleanup
    yaml_document_delete(&document);

    return EXIT_SUCCESS;
}
```

This will create a YAML file containing a sequence of fruits:

```yaml
- Apple
- Orange
- Banana
```

For more in-depth information on using LibYAML, be sure to check out the official documentation and examples.

## See Also

- [LibYAML Documentation](https://pyyaml.org/wiki/LibYAML)
- [Official YAML Website](https://yaml.org/)
- [YAML Tutorial](https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/)