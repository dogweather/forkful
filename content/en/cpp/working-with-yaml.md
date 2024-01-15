---
title:                "Working with yaml"
html_title:           "C++ recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why

If you're a programmer, chances are you've heard of YAML. But why should you care? Simply put, YAML is a popular data serialization format used for storing and representing data in a structured and readable way. This makes it useful for a variety of tasks such as configuration files, data transfer, and integration with other programming languages.

## How To

To start working with YAML in your C++ projects, you'll need a library that supports it. One popular option is the LibYAML library, which allows for easy parsing and generation of YAML files. Here's a simple example of reading a YAML file and printing its contents:

```C++
#include <yaml.h>

// create a parser object
yaml_parser_t parser;

// initialize the parser
yaml_parser_initialize(&parser);

// open the input file
FILE *input_file = fopen("data.yaml", "rb");

// set the input file for the parser
yaml_parser_set_input_file(&parser, input_file);

// create and initialize the event object
yaml_event_t event;
yaml_event_initialize(&event);

// parse the input file
while (yaml_parser_parse(&parser, &event)) {
    // check if the event is a mapping start
    if (event.type == YAML_MAPPING_START_EVENT) {
        // get the next event
        yaml_parser_parse(&parser, &event);
        
        // check if the event is a scalar
        if (event.type == YAML_SCALAR_EVENT) {
            // print the key
            printf("Key: %s\n", event.data.scalar.value);
            
            // get the next event
            yaml_parser_parse(&parser, &event);
            
            // check if the event is a scalar
            if (event.type == YAML_SCALAR_EVENT) {
                // print the value
                printf("Value: %s\n", event.data.scalar.value);
            }
        }
    }
}

// clean up
yaml_parser_delete(&parser);
fclose(input_file);
```
Output:
```
Key: name
Value: John
Key: age
Value: 25
```
Similarly, creating a YAML file is just as easy. Here's an example of creating a YAML file and writing some data to it:

```C++
#include <yaml.h>

// create an emitter object
yaml_emitter_t emitter;

// initialize the emitter
yaml_emitter_initialize(&emitter);

// set the output file
FILE *output_file = fopen("data.yaml", "wb");
yaml_emitter_set_output_file(&emitter, output_file);

// start a YAML document
yaml_emitter_dump(&emitter, YAML_DOCUMENT_START_EVENT);

// start a mapping
yaml_emitter_dump(&emitter, YAML_MAPPING_START_EVENT);

// add a key-value pair
yaml_emitter_dump(&emitter, YAML_SCALAR_EVENT);
yaml_emitter_dump_scalar(&emitter, (yaml_char_t *)"name", strlen("name"));

yaml_emitter_dump(&emitter, YAML_SCALAR_EVENT);
yaml_emitter_dump_scalar(&emitter, (yaml_char_t *)"John", strlen("John"));

// end the mapping
yaml_emitter_dump(&emitter, YAML_MAPPING_END_EVENT);

// end the document
yaml_emitter_dump(&emitter, YAML_DOCUMENT_END_EVENT);

// flush and close the output file
yaml_emitter_flush(&emitter);
fclose(output_file);

// clean up
yaml_emitter_delete(&emitter);
```
Output (data.yaml):
```
---
name: John
```

## Deep Dive

YAML has several built-in data types, including strings, numbers, booleans, and arrays. It also supports comments, making it easy to add notes and explanations to your data. Additionally, YAML is human-readable and can be easily edited without the need for specialized software.

When working with more complex data, YAML also supports references, allowing you to reuse and link to specific data within the file. It also has support for anchors and aliases, which can save space and make your YAML files more efficient.

Overall, YAML provides a flexible and easy-to-use format for storing and representing data, making it a valuable tool for any programmer.

## See Also

- [LibYAML Documentation](https://pyyaml.org/wiki/LibYAML).
- [YAML Basics for Beginners](https://medium.com/@nikhiln47/yaml-basics-for-beginners-9b34a0e1a762).
- [The Official YAML Website](https://yaml.org/).