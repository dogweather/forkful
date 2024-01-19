---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

# How to Print Debug Output in Ruby

## What & Why?

Printing debug output is a diagnostic method where certain values or messages are written (printed) to the console or log. Programmers use it to trace the operational flow of their code and locate errors.

## How to:

Ruby provides an impressive array of tools to facilitate debug print outputs, with `puts`, `p`, and `print` being some of the most commonly used. Let's see them in action:

```Ruby
# Using puts
puts "Hello! This is from puts"
#=> "Hello! This is from puts"

# Using p: identical to puts, but also return the value that gets printed
output = p "Hello! This is from p"
#=> "Hello! This is from p"

# Using print
print "Hello! This is from print"
#=> "Hello! This is from print"
```

The method `puts` (put string) will print an output and add a new line at the end, while `print` does not add a newline. However, `p` is a more feature-rich version of `puts` which returns the output value along with printing it.

For more advanced debugging, turn to Ruby's built-in `debugger`:

```Ruby
# Using debugger
require 'debug'
a = true
b = false
debugger
c = true
```

You can use the `debugger` method to create a breakpoint in code, which will then provide you with a special console, similar to IRB, where you can inspect the values of variables, step through the code or even modify variables.

## Deep Dive

Historically, printing debug output has been an integral part of programming. It's so intrinsic that a 'Hello, World!' program is typically used to illustrate the basic syntax of a programming language for a beginner.

Alternatives to print debugging do exist like using a full debugger. Ruby comes with the ByeBug debugger which can step through code, set conditions and breakpoints, to name a few.

It's important to remember when using print debug to clean up afterwards. Debugging statements left in production code can clutter logs and expose sensitive data.

## See Also

1. [Ruby documentation on IO](https://ruby-doc.org/core-2.7.0/IO.html#method-c-puts)
2. [Byebug debugger for Ruby](https://rubygems.org/gems/byebug)
3. [Ruby documentation on Debugger](https://ruby-doc.org/stdlib-3.0.3/libdoc/debug/rdoc/DEBUGGER__.html)
4. [Thoughtbot post on Ruby Print Debugging](https://thoughtbot.com/blog/print-debugging-ruby)