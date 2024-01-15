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

## Why

JSON, or JavaScript Object Notation, is a widely used format for storing and exchanging data on the web. It has become the de facto standard for transferring data between web applications, making it an essential skill for any developer working with web technologies. By learning how to work with JSON in Ruby, you will be able to process and manipulate data in a seamless and efficient manner.

## How To

Working with JSON in Ruby is simple and straightforward. First, we need to require the JSON library:

```ruby
require 'json'
```

Next, we can convert JSON data into a Ruby object using the `JSON.parse` method:

```ruby
json_string = '{"name": "John", "age": 30}'
user = JSON.parse(json_string)
```

This will convert the JSON data into a hash, which we can then access the values from using standard Ruby hash methods:

```ruby
puts user["name"] #=> John
puts user["age"] #=> 30
```

We can also convert a Ruby object into JSON using the `to_json` method:

```ruby
hash = { "name" => "Jane", "age" => 25 }
puts hash.to_json #=> {"name":"Jane","age":25}
```

## Deep Dive

There are a few important things to keep in mind when working with JSON in Ruby. First, when parsing JSON data, we can use the `symbolize_names` option to convert the keys into symbols instead of strings, making it easier to access the data using symbols:

```ruby
json_string = '{"name": "John", "age": 30}'
user = JSON.parse(json_string, symbolize_names: true)
puts user[:name] #=> John
puts user[:age] #=> 30
```

We can also use the `JSON.pretty_generate` method to format our JSON data in a more readable way, making it easier to debug and understand:

```ruby
hash = { "name" => "Jane", "age" => 25 }
puts JSON.pretty_generate(hash)

#=>
{
  "name": "Jane",
  "age": 25
}
```

Lastly, it's important to handle errors when working with JSON data. Ruby's `JSON.parse` method will raise an error if the JSON data is invalid, so it's best to wrap it in a `begin...rescue` block and handle the error accordingly.

## See Also

- [Official Ruby JSON documentation](https://ruby-doc.org/stdlib-2.7.1/libdoc/json/rdoc/JSON.html)
- [Ruby JSON gems](https://rubygems.org/search?utf8=%E2%9C%93&query=json)
- [Working with JSON in Rails](https://guides.rubyonrails.org/v6.0.3.1/api_app.html#json)