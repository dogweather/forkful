---
title:                "Reading command line arguments"
html_title:           "Ruby recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments in Ruby is the process of retrieving information passed through the command line when executing a Ruby program. Programmers often do this in order to allow for user-input and customization of the program without having to modify the source code each time.

## How to:

The most common way to read command line arguments in Ruby is by using the `ARGV` global variable. Here's an example:

```ruby
# Sample Ruby program for reading command line arguments

# retrieve arguments from command line
args = ARGV

# display the arguments
puts "Your command line arguments are: #{args}"
```

Here's how you would run this program from the command line and supply it with arguments:

```
ruby program.rb first_arg second_arg third_arg
```

Output:
```
Your command line arguments are: ["first_arg", "second_arg", "third_arg"]
```

You can also use the `getopts` method to parse specific arguments and their corresponding values from the command line. Here's an example:

```ruby
# Sample Ruby program for parsing command line arguments

# import the `optparse` library
require 'optparse'

# define a hash to store the options and values
options = {}

# define the options to parse
OptionParser.new do |opts|
  opts.banner = "Usage: program.rb [options]"
  
  opts.on("-u", "--username [USERNAME]", "Specify a username") do |u|
    options[:username] = u
  end
  opts.on("-p", "--password [PASSWORD]", "Specify a password") do |p|
    options[:password] = p
  end
end.parse!

# display the username and password specified
puts "Username: #{options[:username]}"
puts "Password: #{options[:password]}"
```

Here's how you would run this program from the command line and specify options:

```
ruby program.rb -u test_user -p test_pass
```

Output:
```
Username: test_user
Password: test_pass
```

## Deep Dive

The concept of command line arguments has been around since the early days of programming and was first popularized by the C language. In Ruby, the `ARGV` global variable is actually an instance of the `Array` class and contains all the arguments passed through the command line. The `getopts` method is part of the `optparse` library and provides a more organized and customizable way of parsing command line arguments.

Some alternatives to using `ARGV` and `getopts` include using the `ENV` global variable to access environment variables set by the system and using the `reads `gem to provide more advanced argument parsing capabilities.

## See Also

To learn more about command line arguments and their usage in Ruby, check out the following resources:

- [Official Ruby documentation on ARGV](https://ruby-doc.org/core-3.0.0/ARGF.html)
- [Ruby Gems page for the `getopts` library](https://rubygems.org/gems/getopts/versions/1.4.0)
- [Ruby Toolbox page for the `reads `gem](https://www.ruby-toolbox.com/projects/reads)