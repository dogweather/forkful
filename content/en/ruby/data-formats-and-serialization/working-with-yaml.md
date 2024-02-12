---
title:                "Working with YAML"
aliases: - /en/ruby/working-with-yaml.md
date:                  2024-02-03T19:03:11.899120-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML, which stands for YAML Ain't Markup Language, is extensively used in Ruby for configuration files and data serialization due to its human-readable format. Programmers gravitate towards YAML when they need to store or transmit data objects in a readable yet structured manner, simplifying tasks like configuration management, data storage, and inter-language data sharing.

## How to:
Ruby comes with a built-in library called Psych for parsing and emitting YAML. To utilize it, you first need to require the YAML standard library. Here’s a basic example to get you started:

```ruby
require 'yaml'

# Hash to be serialized
person = { name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"] }

# Converting the hash to YAML
yaml_data = person.to_yaml

puts yaml_data
```

**Sample Output:**

```yaml
---
:name: John Doe
:age: 30
:skills:
- Ruby
- JavaScript
```

To load YAML data back into a Ruby object:

```ruby
loaded_person = YAML.load(yaml_data)

puts loaded_person
```

**Sample Output:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

### Using Third-Party Libraries:

Although the standard library is sufficient for basic tasks, for complex needs you might look into third-party gems like 'safe_yaml'. To use such libraries, you must first install the gem:

```bash
gem install safe_yaml
```

Then, you can use it to safely load YAML data, mitigating risks like object instantiation from user-controlled sources:

```ruby
require 'safe_yaml'

safe_loaded_person = SafeYAML.load(yaml_data)

puts safe_loaded_person
```

**Sample Output:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

This approach enhances the security of your YAML handling, making it a good choice for applications that load YAML from untrusted sources.
