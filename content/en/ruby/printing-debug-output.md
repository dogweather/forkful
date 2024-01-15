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

## Why

Ruby is a powerful and versatile programming language that is loved by developers for its simplicity and flexibility. One useful feature of Ruby is the ability to print debug output. This allows developers to see exactly what their code is doing, which can help them troubleshoot and identify any errors in their program.

## How To

Printing debug output in Ruby is as easy as using the "p" or "puts" methods to print out the value of a variable or expression. Let's take a look at a simple example:

```Ruby
a = 5
b = 10
puts a + b
```

The output of this code would be "15", as the "puts" method prints the result of the addition operation. Similarly, we could use the "p" method to print out the values of variables:

```Ruby
a = 5
b = 10
p a, b
```

This would print out the values of both "a" and "b" as they are, without performing any operations on them. In addition to printing out values, you can also use these methods to print out messages for debugging purposes. For example:

```Ruby
a = 5
b = 10
puts "The value of a is #{a} and the value of b is #{b}"
```

The output of this code would be "The value of a is 5 and the value of b is 10", providing useful information for troubleshooting. Overall, printing debug output in Ruby is a simple and effective way to gain insights into your code and fix any issues that may arise.

## Deep Dive

Now that you know how to print debug output in Ruby, let's take a deeper dive into some other useful ways to use this feature. In addition to the "p" and "puts" methods, there are also the "pp" and "print" methods. The "pp" method prints out the value of a variable in a more readable format, while the "print" method does not add a new line after printing, allowing for more precise formatting.

Another useful technique is to use the "inspect" method on an object to print out its properties and values. This is especially helpful when working with complex data structures such as arrays or hashes. Additionally, you can use the "-d" flag when running a Ruby file to activate debug mode and automatically print out all executed lines of code, making it even easier to track the flow of your program.

## See Also

For more information on printing debug output in Ruby, check out the following resources:

- [Ruby Documentation](https://ruby-doc.org/core-2.7.2/Kernel.html#method-i-p)
- [Debugging in Ruby](https://medium.com/better-programming/debugging-in-ruby-using-pry-ce6475215e62)
- [Ruby Debugging Tips](https://www.rubyguides.com/2015/09/ruby-debugging-tips/)

So go ahead and start incorporating debug output into your Ruby code to enhance your development process! Happy coding!