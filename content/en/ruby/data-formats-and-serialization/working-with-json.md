---
title:                "Working with JSON"
aliases: - /en/ruby/working-with-json.md
date:                  2024-02-03T19:03:12.971076-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

JSON (JavaScript Object Notation) is a lightweight data interchange format, prevalent in web applications for data exchange between clients and servers. Programmers work with JSON in Ruby to parse data received from external sources or to format data for API responses, leveraging its human-readable structure for easy data manipulation and storage.

## How to:

Ruby, with its standard library, provides seamless ways to parse and generate JSON. The primary module for these operations is `json`, which can be easily integrated into any Ruby application.

### Parsing JSON:

To convert a JSON string to a Ruby hash, you can use the `JSON.parse` method.

```ruby
require 'json'

json_string = '{"name": "John Doe", "age": 30, "city": "New York"}'
ruby_hash = JSON.parse(json_string)

puts ruby_hash
# Output: {"name"=>"John Doe", "age"=>30, "city"=>"New York"}
```

### Generating JSON:

Conversely, to convert a Ruby hash into a JSON string, you use the `JSON.generate` method or the `to_json` method available on Ruby objects once the `json` library is required.

```ruby
require 'json'

ruby_hash = { name: "Jane Doe", age: 25, city: "Los Angeles" }
json_string = ruby_hash.to_json

puts json_string
# Output: {"name":"Jane Doe","age":25,"city":"Los Angeles"}
```

### Third-party Libraries:

While Ruby's standard library covers basic JSON handling, many projects rely on third-party libraries for enhanced functionality and performance. One popular choice is `Oj` (Optimized JSON).

#### Parsing with Oj:

```ruby
require 'oj'

json_string = '{"name": "Alex", "age": 40, "city": "Chicago"}'
ruby_hash = Oj.load(json_string)

puts ruby_hash
# Output: {"name"=>"Alex", "age"=>40, "city"=>"Chicago"}
```

#### Generating with Oj:

Oj also offers a fast way to generate JSON from Ruby objects:

```ruby
require 'oj'

ruby_hash = { name: "Samantha", age: 35, city: "Miami" }
json_string = Oj.dump(ruby_hash)

puts json_string
# Output: {"name":"Samantha","age":35,"city":"Miami"}
```

These examples illustrate the straightforward nature of working with JSON in Ruby, making it accessible for tasks ranging from simple data manipulations to complex API communications.
