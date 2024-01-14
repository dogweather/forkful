---
title:                "Ruby recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/working-with-json.md"
---

{{< edit_this_page >}}

# Why
Working with JSON (JavaScript Object Notation) in Ruby can be incredibly useful when exchanging data between different systems or applications. It provides a lightweight and readable format for data, making it easier to parse and manipulate compared to other data formats.

# How To
Using Ruby, there are a few different ways to work with JSON. The first option is to use the built-in `JSON` module. This provides a simple and straightforward way to convert objects to JSON format and vice versa. Here's an example of how to convert a hash to JSON:

```Ruby
require 'json'

my_hash = { name: "John", age: 30 }
json_data = my_hash.to_json

puts json_data # Outputs: {"name": "John", "age": 30}
```

You can also convert JSON data into a Ruby hash using the `JSON.parse` method:

```Ruby
require 'json'

json_data = '{"name": "John", "age": 30}'
my_hash = JSON.parse(json_data)

puts my_hash # Outputs: {:name=>"John", :age=>30}
```

Another way to work with JSON in Ruby is by using the popular `HTTParty` gem. This allows you to make HTTP requests and handle the response in JSON format. Here's an example of how to make a GET request and retrieve JSON data from an API:

```Ruby
require 'httparty'

response = HTTParty.get('https://api.example.com/users')
users = response.parsed_response

puts users # Outputs: [{"name": "John", "age": 30}, {"name": "Jane", "age": 25}]
```

# Deep Dive
When working with JSON in Ruby, it's important to understand the methods provided by the `JSON` module. Some useful methods include `generate`, `parse`, `pretty_generate`, `load`, and `dump`. These methods allow you to convert between JSON and Ruby objects, as well as format the JSON data for readability.

In addition, it's important to be familiar with the structure of JSON data. It consists of key-value pairs enclosed in curly braces, with data types such as strings, numbers, arrays, and objects. It's also worth noting that JSON supports nested objects and arrays, making it a versatile format for representing complex data.

# See Also
- [Ruby's `JSON` module documentation](https://ruby-doc.org/stdlib-2.7.2/libdoc/json/rdoc/JSON.html)
- [HTTParty gem documentation](https://github.com/jnunemaker/httparty)
- [JSON data structures](https://www.json.org/json-en.html)