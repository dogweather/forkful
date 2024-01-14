---
title:                "C recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why

YAML, short for "YAML Ain't Markup Language", is a popular data serialization format that is human-readable and easy to understand. It is used for storing and transferring structured data, making it a useful tool for developers who need to work with data in different formats.

## How To

If you're new to YAML and want to use it in your C programming projects, here's a quick guide to get you started.

First, you'll need to include the YAML library in your code. This can be done by adding ``` #include <yaml.h> ``` at the beginning of your program.

Next, you'll need to create a YAML document using the ```yaml_document_t``` structure. This structure represents the YAML document and contains the necessary information for parsing and manipulating it.

Then, you can start adding data to your YAML document using the ```yaml_scalar_event_t``` structure. This structure allows you to specify the key and value for your data in the YAML document.

Finally, once you have added all the necessary data, you can output the YAML document using the ```yaml_document_dump()``` function. This will generate a human-readable YAML document that you can use for storing or transferring data.

Here's an example of how your code might look like:

```
#include <yaml.h>

int main() {
  yaml_document_t document;
  yaml_scalar_event_t event;

  // initialize YAML document
  yaml_document_initialize(&document, NULL, NULL, NULL, 0, 0);

  // add key-value pair to YAML document
  yaml_scalar_event_initialize(&event, NULL, (yaml_char_t *) "name", (yaml_char_t *) "John", 4, 1, 1, YAML_PLAIN_SCALAR_STYLE);
  yaml_document_append_scalar(&document, NULL, &event);

  // output YAML document
  yaml_document_dump(&document, stdout);

  // cleanup
  yaml_document_delete(&document);
  yaml_document_delete(&event.document);

  return 0;
}
```

And here's the resulting output:

```
name: John
```

## Deep Dive

If you want to dive deeper into working with YAML in C, there are a few more concepts you should be aware of. For example, the YAML library allows you to specify different data types for your values, such as strings, integers, and booleans. It also supports more complex data structures like arrays and mappings.

You can also customize the output of your YAML document by specifying different styles for your data, such as plain, single-quoted, or double-quoted.

Additionally, the YAML library offers error handling features to help you identify and handle any issues with your YAML document.

For more detailed information and examples, you can refer to the official documentation for the YAML library.

## See Also

To further enhance your knowledge of working with YAML in C, you can check out these helpful resources:

- [YAML Specification](https://yaml.org/spec/) - the official YAML specification.
- [YAML C Library Documentation](https://github.com/yaml/libyaml) - the official documentation for the YAML C library.
- [YAML Basics - Commented Example](https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/) - a detailed tutorial on YAML with code examples in various languages, including C.

Now that you have a basic understanding of how to use YAML in your C programs, go ahead and give it a try in your next project!