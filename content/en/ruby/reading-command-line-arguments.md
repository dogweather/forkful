---
title:                "Ruby recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Reading command line arguments may seem like a small aspect of Ruby programming, but it can greatly enhance the functionality and versatility of your code. Command line arguments allow users to provide input directly from the terminal, making your programs more user-friendly and customizable. 

## How To

To read command line arguments in Ruby, we can use the "ARGV" variable, which contains an array of strings representing the arguments passed in from the command line. Let's take a look at an example:

```Ruby
# Running this program in the terminal with the following command:
# ruby read_arguments.rb Hello World

ARGV.each do |arg|
  puts "Argument passed in: " + arg
end
```

In this example, we have used the "each" method to iterate through the "ARGV" array and print out each argument that was passed in. When we run this program in the terminal with the command "ruby read_arguments.rb Hello World", we will get the following output:

```Console
Argument passed in: Hello
Argument passed in: World
```

We can also access individual arguments by their index in the "ARGV" array. For example, if we wanted to access the first argument "Hello", we would use "ARGV[0]". Keep in mind that the first argument is always located at index 0, not 1.

## Deep Dive

In addition to reading command line arguments with the "ARGV" variable, we can also use the "OptionParser" class to handle more complex arguments. This allows us to define options and flags with specific behaviors, making our programs even more dynamic. Let's see how this works:

```Ruby
require 'optparse'

options = {}

OptionParser.new do |opts|
  opts.banner = "Usage: read_arguments.rb [options]"

  opts.on("-s", "--string STRING", "Specify a string") do |string|
    options[:string] = string
  end

  opts.on("-n", "--number NUMBER", Integer, "Specify a number") do |number|
    options[:number] = number
  end

  opts.on("-f", "--flag", "Specify a flag") do |flag|
    options[:flag] = flag
  end
end.parse!

puts "String: " + options[:string] if options[:string]
puts "Number: " + options[:number].to_s if options[:number]
puts "Flag: " + options[:flag].to_s if options[:flag]
```

In this example, we have defined three options: a string, a number, and a flag. We can specify these options when running our program in the terminal, for example, using the command "ruby read_arguments.rb -s Hello -n 123 -f". This would output:

```Console
String: Hello
Number: 123
Flag: true
```

We can also add descriptions for each option, making our program more user-friendly and providing guidance for users who may not be familiar with the program. This is just one example of how we can take a deeper dive into reading command line arguments with Ruby. 

## See Also

For more information on reading command line arguments in Ruby and using the "OptionParser" class, check out these helpful resources:

- [Ruby-Doc: ARGV](https://ruby-doc.org/core-3.0.1/ARGF.html)
- [Ruby-Doc: OptionParser](https://ruby-doc.org/stdlib-3.0.1/libdoc/optparse/rdoc/OptionParser.html)
- [Tutorial: Reading command line arguments in Ruby](https://www.tutorialspoint.com/ruby/ruby_command_line.htm)