---
date: 2024-01-20 17:56:37.926451-07:00
description: "Reading command-line arguments in Ruby allows scripts to take input\
  \ right when they're run, like configuring options or passing data. Programmers\
  \ use them\u2026"
lastmod: '2024-03-13T22:45:00.565435-06:00'
model: gpt-4-1106-preview
summary: "Reading command-line arguments in Ruby allows scripts to take input right\
  \ when they're run, like configuring options or passing data. Programmers use them\u2026"
title: Reading command line arguments
---

{{< edit_this_page >}}

## What & Why?
Reading command-line arguments in Ruby allows scripts to take input right when they're run, like configuring options or passing data. Programmers use them to make scripts dynamic and adaptable without hardcoding values.

## How to:
To grab command-line arguments, Ruby provides a simple array: `ARGV`. It contains all the arguments passed, in the order they were given.

```Ruby
# hello.rb
name = ARGV[0] || "World"
puts "Hello, #{name}!"

# Run with: ruby hello.rb Alice
# Output: Hello, Alice!
```

To handle multiple arguments:

```Ruby
# greet.rb
name, time_of_day = ARGV
puts "Good #{time_of_day || 'day'}, #{name || 'there'}!"

# Run with: ruby greet.rb Bob Morning
# Output: Good Morning, Bob!
```

Create options with a loop:

```Ruby
# options.rb
options = {}
ARGV.each do |arg|
  key, value = arg.split('=')
  options[key.to_sym] = value
end
p options

# Run with: ruby options.rb name=Tom age=30
# Output: {:name=>"Tom", :age=>"30"}
```

## Deep Dive
Reading command-line arguments is a practice as old as command-line interfaces themselves. It's about utilizing user input without GUI—essential for automation or when running scripts on servers.

Ruby's `ARGV` is not unique; many languages have something similar. Yet, Ruby's implementation leans on simplicity and clear syntax—no fuss, just an array.

Beneath the surface, `ARGV` is just an instance of `Array` pre-populated with the arguments that appear after the script name in the command call. Ruby sets it up before your code even runs, making it immediately ready for use.

Alternatives? Sure. For complex needs, like parsing flags (e.g., `--verbose` or `-v`), Ruby has the `OptionParser` class in the standard library. This can handle more than `ARGV`, like default values, automatic type conversion, and generating help messages.

Sometimes, you just want to know if an argument was provided or not, ignoring its value. For that, `ARGV.include?` does the trick.

## See Also
- An intro to `OptionParser`: [https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html](https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html)
- More on command-line arguments in Ruby: [https://www.rubyguides.com/2018/12/ruby-argv/](https://www.rubyguides.com/2018/12/ruby-argv/)
