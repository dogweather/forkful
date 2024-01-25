---
title:                "Working with YAML"
html_title:           "Arduino recipe: Working with YAML"
simple_title:         "Working with YAML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML stands for "YAML Ain't Markup Language." It's a human-readable data serialization format. Programmers use it for config files, data exchange between languages, and because it's more readable than JSON or XML for complex data structures.

## How to:

To work with YAML in Ruby you need the `yaml` library. It's part of Ruby's standard library, so just require it:

```ruby
require 'yaml'
```

To dump a Ruby hash to a YAML string:

```ruby
require 'yaml'

my_hash = { name: 'Sam', occupation: 'Developer', hobbies: ['coding', 'chess'] }

yaml_string = my_hash.to_yaml
puts yaml_string
```

Output will be a YAML-formatted string:

```
---
:name: Sam
:occupation: Developer
:hobbies:
- coding
- chess
```

To load a YAML string into Ruby:

```ruby
require 'yaml'

yaml_string = "
name: Sam
occupation: Developer
hobbies:
  - coding
  - chess
"

ruby_hash = YAML.load(yaml_string)
puts ruby_hash
```

Output is a Ruby hash:

```
{name: 'Sam', occupation: 'Developer', hobbies: ['coding', 'chess']}
```

## Deep Dive

YAML emerged in the early 2000s as a human-friendly alternative to XML for config files and data serialization. Its design allows easy mapping to native data structures in many languages, having implementations in Python, Ruby, Java, PHP, and others.

Alternatives to YAML include JSON and TOML. JSON is more common for web APIs due to its direct compatibility with JavaScript. TOML aims to be more readable as a config file while offering a similar feature set as YAML.

In Ruby, YAML is implemented by the Psych library, which has been the default YAML parser since Ruby 1.9.3. Psych interacts with libyaml, a C library for YAML parsing and emitting.

## See Also

- [The Official YAML Website](https://yaml.org/)
- [Psych Library Documentation](https://ruby-doc.org/stdlib-3.0.0/libdoc/psych/rdoc/Psych.html)
- [Ruby YAML Module Documentation](https://ruby-doc.org/stdlib-2.5.1/libdoc/yaml/rdoc/YAML.html)
- [JSON (JavaScript Object Notation) Official Site](https://www.json.org/)
- [TOML (Tom's Obvious, Minimal Language) GitHub Repository](https://github.com/toml-lang/toml)