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

## Why

If you're a Ruby programmer, chances are, you've encountered a situation where you needed to interact with a user through the command line. Reading command line arguments is essential for building interactive command line tools or for handling user inputs in your scripts. In this article, we'll explore how to read command line arguments in Ruby and how it can make your code more dynamic and user-friendly.

## How To

In Ruby, reading command line arguments is made easy with the `ARGV` array. This array contains all the arguments passed to your script when it is executed. Let's take a look at a simple example:

```Ruby
# demo.rb
puts "Hello, #{ARGV[0]}!"
```

When we execute this script with `ruby demo.rb world`, the output will be `Hello, world!`. Here, `ARGV[0]` refers to the first argument passed in, which in this case is "world". 

You can also access multiple arguments by using their respective indexes in the `ARGV` array. For example, if we run `ruby demo.rb hello world`, the output will be `Hello, hello world!`, since `ARGV[0]` refers to the first argument "hello" and `ARGV[1]` refers to the second argument "world". 

Another useful feature is the ability to pass in arguments with flags using the `-` symbol. These arguments can be accessed with the `ARGV.shift` method. For example, if we run `ruby demo.rb -greeting hello`, the output will be `Hello, hello!` since `ARGV.shift` will return the first argument after the flag "-greeting".

## Deep Dive

You may have noticed that `ARGV` is an array, which means it has all the array methods available to use. This makes it easy to manipulate the arguments passed in to fit your needs. You can also explicitly convert the arguments to different data types using methods like `to_i` or `to_f` for integers and floats, respectively.

It is also important to note that the `ARGV` array is not limited to just strings. It can also contain any valid data type, including arrays and hashes. This can be useful for passing in complex data structures as command line arguments.

## See Also

- [Ruby ARGV Documentation](https://ruby-doc.org/core-2.7.1/ARGV.html)
- [Command Line Arguments in Ruby - Medium Article](https://medium.com/rubycademy/reading-command-line-arguments-in-ruby-d662d7b2c85b)
- [Ruby ARGV Tutorial - YouTube Video](https://www.youtube.com/watch?v=0us5PINRUZM)