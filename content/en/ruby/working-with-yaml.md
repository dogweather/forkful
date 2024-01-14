---
title:                "Ruby recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why
Ruby developers often encounter the need to work with configuration files, and YAML (Yet Another Markup Language) is a popular choice for this task. YAML files are human-readable and easy to work with, making them a valuable tool for managing data in a structured format.

## How To
To get started with YAML in Ruby, you will need to install the 'yaml' module using the following command in your terminal:

`gem install yaml`

Once the module is installed, you can begin working with YAML files in your Ruby code. Let's take a look at an example of how to read and write data to a YAML file:

```Ruby
require 'yaml'

# Reading data from a YAML file
data = YAML.load_file('data.yml')
puts data # Output: {"name"=>"John Doe", "age"=>30}

# Writing data to a YAML file
new_data = {"name" => "Jane Smith", "age" => 25}
File.open('new_data.yml', 'w') do |f|
  f.write(new_data.to_yaml)
end
```

In the above example, we used the `load_file` method to read data from a YAML file and the `to_yaml` method to write data to a YAML file. Notice how the data is structured in a key-value format, similar to a hash in Ruby. This makes it easy to work with and modify the data as needed.

YAML also allows for more complex data structures, such as arrays and nested hashes. Here's an example of a YAML file with nested data:

```Ruby
# YAML file
- name: John Doe
  age: 30
  hobbies:
    - reading
    - hiking
- name: Jane Smith
  age: 25
  hobbies:
    - yoga
    - painting
```

To access the data in a nested structure, you can use the `[]` method and specify the key of the data you want to access. For example, `data[0]['name']` would return "John Doe" in the above example.

## Deep Dive
YAML may seem simple, but it offers many features that make it a powerful tool for managing data. For instance, YAML allows for data references, which allow you to reuse the same data at multiple places in your code. This can be especially useful when working with large and complex data sets.

Another useful feature of YAML is the ability to set default values for keys. This means that if a key is not present in a data file, it will use the default value specified in the YAML code. This can help to ensure that your code runs smoothly even if certain data is missing.

YAML also has support for comments, making it easier to document your data and code. Comments in YAML are denoted by the `#` symbol, similar to Ruby.

## See Also
For more information on working with YAML in Ruby, check out these resources:

- [Ruby documentation on YAML](https://ruby-doc.org/stdlib-3.0.2/libdoc/yaml/rdoc/YAML.html)
- [YAML specification](https://yaml.org/spec/)
- [YAML tutorial by Tutorialspoint](https://www.tutorialspoint.com/yaml/)