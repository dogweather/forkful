---
date: 2024-01-25 03:39:32.458446-07:00
description: "TOML is a config file format that's easy to read due to its clear semantics.\
  \ Programmers use TOML to manage app configs and data serialization without the\u2026"
lastmod: 2024-02-19 22:05:19.033335
model: gpt-4-1106-preview
summary: "TOML is a config file format that's easy to read due to its clear semantics.\
  \ Programmers use TOML to manage app configs and data serialization without the\u2026"
title: Working with TOML
---

{{< edit_this_page >}}

## What & Why?

TOML is a config file format that's easy to read due to its clear semantics. Programmers use TOML to manage app configs and data serialization without the heft of XML or quirks of YAML.

## How to:

First, install the `toml-rb` gem. It's a popular choice for TOML parsing in Ruby.

```Ruby
gem install toml-rb
```

Next, reading a TOML file:

```Ruby
require 'toml-rb'

toml_content = File.read('config.toml')
config = TomlRB.parse(toml_content)
puts config['title']
```

Sample output might be:

```
My Awesome App
```

Writing to a TOML file:

```Ruby
require 'toml-rb'

config = {
  'title' => 'My Awesome App',
  'owner' => {
    'name' => 'John Doe',
    'dob' => Date.new(1979, 5, 27)
  }
}

toml_string = TomlRB.dump(config)
File.write('config.toml', toml_string)
```

Check `config.toml` and you'll see your settings, neatly stored.

## Deep Dive

TOML, which stands for Tom's Obvious, Minimal Language, was created by Tom Preston-Werner, the co-founder of GitHub, around 2013. Its primary goal is to be a straightforward format that's easy to parse into data structures. While JSON is great for APIs, and YAML is flexible, TOML's niche is its emphasis on being human-friendly. Unlike YAML, which can be finicky with indentation, TOML aims for a more INI-like structure which many find simpler and less error-prone.

Alternatives like JSON, YAML, or XML each have their own strengths, but TOML thrives in scenarios where a config should be easily maintained by humans and programs alike. It's not only simpler but enforces strict and readable formatting.

On the technical side, to parse TOML content with Ruby, we leverage gems like `toml-rb`. This gem takes advantage of Ruby's dynamic nature, converting TOML data into native Ruby hashes, arrays, and other basic data structures. This conversion means that developers can work with TOML data using familiar Ruby semantics and methods.

## See Also

- TOML project and spec: https://toml.io/en/
- The `toml-rb` gem: https://github.com/emancu/toml-rb
- Comparing TOML, YAML, and JSON: https://blog.theodo.com/2021/08/compare-yml-toml-json/
