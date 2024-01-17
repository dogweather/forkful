---
title:                "Printing debug output"
html_title:           "Ruby recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output is the process of displaying the value of a variable or the result of an expression in a program's code for troubleshooting purposes. Programmers use it to understand and fix errors or bugs in their code.

## How to:

To print debug output in Ruby, we can use the `puts` method. For example, let's say we have the following code:

```
name = "John"
puts "Hello " + name
```

The output of this code would be `Hello John`, because the `puts` method will print out whatever follows it, in this case, the result of concatenating the string "Hello " with the value of the `name` variable.

We can also use the `p` method to print debug output. Let's continue with our previous example:

```
name = "John"
p "Hello " + name
```

The output of this code would be `"Hello John"`, as the `p` method will display the exact value of the expression it is given, including any quotes.

## Deep Dive:

Printing debug output is a common practice in programming, dating back to the early days of computing. Before the existence of advanced debugging tools, developers would use print statements to get a better understanding of what was happening in their code.

Besides using the `puts` and `p` methods, we can also use Ruby's `print` method, which behaves similarly to `puts` but does not add a new line character at the end. This can be useful when we want to display multiple values on the same line.

Another alternative to printing debug output is using a debugger tool, such as Pry or Byebug, which allows for more advanced debugging techniques like setting breakpoints and inspecting variables within the code.

## See Also:

- [Ruby Method: puts vs. print vs. p](https://www.rubyguides.com/2018/10/puts-vs-print-vs-p-in-ruby/)
- [Debugging in Ruby using Pry](https://www.rubyguides.com/2016/07/debugging-pry-ruby/)
- [Byebug Debugging Cheatsheet](https://gist.github.com/jmoses/1276030)