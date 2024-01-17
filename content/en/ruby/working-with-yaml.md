---
title:                "Working with yaml"
html_title:           "Ruby recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

# Working with YAML in Ruby 

## What & Why?
YAML (YAML Ain't Markup Language) is a simple data serialization language, widely used in programming for storing and exchanging data in a human-readable format. It is often used to easily store configuration settings or structured data for applications. Programmers use YAML for its simplicity, readability, and flexibility, making it a popular choice for storing data in Ruby projects.

## How to:
To use YAML in Ruby, first, we need to require the `yaml` library in our code. Then, we can use the `load` and `dump` methods to convert data to and from YAML format. Let's take a look at an example:

```Ruby
require 'yaml'

# Create a hash object
person = { name: "John", age: 30, hobbies: ["coding", "reading", "playing guitar"] }

# Convert to YAML format
yaml_person = person.to_yaml

# Print YAML output
puts yaml_person

# Convert YAML back to hash
hash_person = YAML.load(yaml_person)

# Print hash output
puts hash_person
```

Output:
```yaml
---
:name: John
:age: 30
:hobbies:
  - coding
  - reading
  - playing guitar
```
```ruby
{:name=>"John", :age=>30, :hobbies=>["coding", "reading", "playing guitar"]}
```

As you can see, the `to_yaml` method converts the hash object into a string in valid YAML format, while the `load` method converts the YAML string back into a Ruby hash.

## Deep Dive:
YAML was first designed in 2001 and is a human-friendly, cross-platform, programming language independent format. It is widely used in various applications and can also be used for configuration files, providing a more readable alternative to traditional formats like XML or JSON. Alternatively, other options for storing data in Ruby include using built-in data structures like arrays or hashes.

One key feature of YAML is that it supports comments, making it easier for developers to add notes or explanations for each data entry. This makes the format more user-friendly and organized compared to others.

The implementation of YAML in Ruby is done through the `Psych` library, which is part of the standard library since Ruby 1.9.3. This library can be extended to support custom objects, making it even more versatile for data serialization.

## See Also:
- [YAML Specification](https://yaml.org/spec/1.2/spec.html)
- [Psych Library Documentation](https://ruby-doc.org/stdlib-2.6.3/libdoc/psych/rdoc/Psych.html)
- [YAML vs. JSON: Which is Best for Configuration Files?](https://deliciousbrains.com/yaml-vs-json/)