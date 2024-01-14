---
title:                "Ruby recipe: Reading command line arguments"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Reading command line arguments may seem like a daunting task for beginner programmers, but it is an essential skill to have in order to create more dynamic and interactive programs. By being able to read and manipulate command line arguments, you can add a whole new level of customization and versatility to your Ruby programs. In this blog post, we will explore the ins and outs of reading command line arguments in Ruby and why it is an important skill for any programmer to have.

## How To

Reading command line arguments in Ruby is actually quite simple. Let's take a look at a basic example:

```
# program.rb

# gets the first command line argument and stores it in a variable
user_name = ARGV[0]

# prints a personalized greeting for the user
puts "Hello, #{user_name}! Welcome to my program."
```

To run this program, we would type the following into our terminal:

```
ruby program.rb John
```

The output would then be:

```
Hello, John! Welcome to my program.
```

As you can see, the first command line argument after the name of the program (in this case "John") is being stored in the variable `user_name` and is being used to create a personalized greeting. 

But what if we want to have multiple command line arguments? We can simply add more elements to the `ARGV` array and access them by their index. For example:

```
# program.rb

# gets the first and last command line arguments and stores them in variables
first_name = ARGV[0]
last_name = ARGV[1]

# prints a personalized message for the user
puts "Welcome, #{first_name} #{last_name}! We hope you enjoy this program."
```

To run this program, we would type the following into our terminal:

```
ruby program.rb John Smith
```

The output would then be:

```
Welcome, John Smith! We hope you enjoy this program.
```

Command line arguments can also be very useful when dealing with user input. Instead of asking the user to input their name every time, we can simply use a command line argument to save time and make our program more efficient.

## Deep Dive

Within our Ruby programs, we have access to a special array called `ARGV` that contains all of the command line arguments entered after the name of the program. This array is zero-indexed, meaning the first command line argument will be at index 0, the second at index 1, and so on. We can use the `ARGV` array just like any other array in Ruby, meaning we can use methods like `length` and `each` to manipulate it.

It's important to note that command line arguments will always be read in as strings, so if you need to convert them to a different data type (such as an integer or a float), you will need to use methods like `to_i` or `to_f`.

Another useful feature when working with command line arguments is the ability to use flags. Flags are designated by a `-` or `--` before the argument and are used to specify certain options or configurations for the program. For example, running `ruby program.rb -v` could be used to display the version of the program.

## See Also

For more information on reading command line arguments in Ruby, check out these helpful resources:

- [Official Ruby documentation](https://ruby-doc.org/core-2.7.1/ARGF.html)
- [Command line arguments in Ruby](https://www.rubyguides.com/2019/05/ruby-command-line-arguments/)
- [Passing command line arguments in Ruby](https://medium.com/@bdov_/passing-command-line-arguments-in-ruby-4020f262c893)