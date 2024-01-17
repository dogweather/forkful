---
title:                "Working with json"
html_title:           "Ruby recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

Working with JSON in Ruby involves parsing and generating data in JSON format using the `json` library. This is a common practice among programmers as JSON is a lightweight and flexible data interchange format, making it ideal for transferring data between different systems and languages.

## How to:

To work with JSON in Ruby, we first need to require the `json` library. Then, we can use the `parse` and `generate` methods to convert data to and from JSON format.

```ruby
require 'json'

# Parse JSON to Ruby object
json_string = '{"name": "John", "age": 25}'
ruby_object = JSON.parse(json_string)
puts ruby_object["name"] # Output: John

# Generate JSON from Ruby object
ruby_object = {name: "Jane", age: 30}
json_string = JSON.generate(ruby_object)
puts json_string # Output: {"name":"Jane","age":30}
```

## Deep Dive:

JSON (JavaScript Object Notation) was first introduced in 2001 as a lightweight alternative to XML. It quickly gained popularity in web development due to its simplicity and human-readable syntax. Ruby's `json` library is part of its standard library since version 1.9, making it accessible to all Ruby programmers without the need for external dependencies.

An alternative to `json` library is the `oj` gem which targets improving performance and memory consumption. It also provides additional features such as optimizing JSON generation for Ruby objects.

When working with JSON in Ruby, it's important to keep in mind that JSON supports the following data types: null, boolean, number, string, array, and object. The `json` library automatically converts these data types between Ruby and JSON, making it easy to work with.

## See Also:

- [Official Ruby JSON Library Docs](https://ruby-doc.org/stdlib-2.7.2/libdoc/json/rdoc/JSON.html)
- [oj gem Docs](https://www.rubydoc.info/gems/oj)
- [JSON Spec](https://www.json.org/json-en.html)