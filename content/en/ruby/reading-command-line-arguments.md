---
title:    "Ruby recipe: Reading command line arguments"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Why
As a developer, it's important to have a clear understanding of how your program is being used by its users. One way to achieve this is by using command line arguments. By reading these arguments, your program can receive input from the user before execution, making it more versatile and user-friendly. In this blog post, we'll dive into the world of reading command line arguments in Ruby.

## How To
To start off, let's look at how we can read command line arguments in Ruby. First, we need to access the `ARGV` variable, which contains an array of the arguments passed in by the user. Here's an example:

```ruby
# command_line_args.rb
puts ARGV
```

If we run this program with the command `ruby command_line_args.rb hello world`, the output would be `["hello", "world"]`, with each argument separated by a space. We can also access individual arguments by indexing the `ARGV` array, like this:

```ruby
# command_line_args.rb
puts ARGV[0]
puts ARGV[1]
```

The output for this program, using the same command as before, would be:
```
hello
world
```

Additionally, we can use methods like `.length` to find out how many arguments were passed in, and `.join` to merge the arguments into a single string. Here's an example:

```ruby
# command_line_args.rb
puts "Number of arguments: #{ARGV.length}"
puts "Arguments: #{ARGV.join(" ")}"
```
Running this program with `ruby command_line_args.rb I love Ruby programming` would output:
```
Number of arguments: 4
Arguments: I love Ruby programming
```

## Deep Dive
Now that we know how to read and manipulate command line arguments in Ruby, let's take a deeper look into how it works. Behind the scenes, when we run a Ruby program with arguments, our operating system sends the arguments as a string to the `ARGV` array. This is why we can use methods like `.join` on the `ARGV` array, just like we would with any other string.

It's also worth mentioning that arguments are separated by spaces, unless we use quotation marks (" ") to group them together as a single argument. For example, if we run `ruby command_line_args.rb "Hello World"`, the output would be `["Hello World"]` instead of `["Hello", "World"]`.

In addition, we can also use the `-s` command line option when running a Ruby program to treat the arguments as switches, converting them into a hash with their corresponding values. Essentially, this allows us to create options for our program. Here's an example:

```ruby
# command_line_args.rb
require 'optparse'

options = {}
OptionParser.new do |opts|
  opts.on('-n', '--name NAME', 'Display the user\'s name') do |name|
    options[:name] = name
  end
  opts.on('-l', '--language LANGUAGE', 'Display the user\'s favorite programming language') do |language|
    options[:language] = language
  end
end.parse!

puts "Hello, #{options[:name]}! I see that your favorite programming language is #{options[:language]}."
```

Running this program with the command `ruby command_line_args.rb -n John -l Ruby` would output:
```
Hello, John! I see that your favorite programming language is Ruby.
```

## See Also
- [Ruby Command Line Arguments](https://ruby-doc.org/docs/ProgrammingRuby/html/tut_stdtypes.html#S6)
- [Ruby OptionParser](https://ruby-doc.org/stdlib-2.5.1/libdoc/optparse/rdoc/OptionParser.html)
- [Using ARGV in Ruby](https://www.rubyguides.com/2018/05/argv-command-line-arguments/)

By understanding how to read command line arguments in Ruby, we can create more dynamic and interactive programs. So why not give it a try in your next project? Happy coding!