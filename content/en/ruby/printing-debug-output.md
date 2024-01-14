---
title:                "Ruby recipe: Printing debug output"
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why
Debugging is an essential part of any programming project, and being able to print out debug output is a useful tool in every developer's arsenal. It allows you to see the flow of your code, spot errors, and test different scenarios. In this blog post, we will go over the basics of printing debug output in Ruby, including why it's beneficial and how to do it.

## How To

To print out debug output in Ruby, we use the `puts` method to display the desired information in the console. Let's take a look at a simple example:

```Ruby
def multiply(num1, num2)
  puts "Multiplying #{num1} and #{num2}"
  return num1 * num2
end

multiply(5, 10)
```

This code will output the following in the console:

```
Multiplying 5 and 10
=> 50
```

As you can see, we used string interpolation to include the values of `num1` and `num2` in our output. This can be helpful when trying to figure out the values of variables or the result of a calculation.

We can also use the `p` method to print out the value of a variable:

```Ruby
age = 25
p age
```

This will output `25` in the console. The `p` method is useful when dealing with more complex data structures like arrays or hashes.

Another useful tool for printing debug output is the `logger` class. It allows you to log information at different levels, such as `debug`, `info`, `warn`, `error`, and `fatal`. Here's an example of how to use it:

```Ruby
require 'logger'

logger = Logger.new(STDOUT)

logger.debug("Debug message")
logger.info("Info message")
```

This will output the following in the console:

```
D, [2020-01-01T12:00:00.000000 #12345] DEBUG -- : Debug message
I, [2020-01-01T12:00:00.000000 #12345]  INFO -- : Info message
```

For more information on the `logger` class and its capabilities, check out the official documentation [here](https://guides.rubyonrails.org/debugging_rails_applications.html#the-logger).

## Deep Dive

Printing debug output is not only useful for troubleshooting and fixing errors but can also help newcomers understand the flow of a codebase. It allows them to see the values of variables and the logic behind each step. However, it's essential to use debug output sparingly and to remove it before deploying your code to production.

Besides using `puts`, `p`, and `logger`, there are other tools and techniques for printing debug output, such as using a debugger or writing custom print methods. It's up to you to decide which method works best for your specific scenario.

## See Also
- [Official Ruby Documentation](https://www.ruby-doc.org/stdlib-2.7.2/libdoc/logger/rdoc/Logger.html)
- [Debugging Rails Applications](https://guides.rubyonrails.org/debugging_rails_applications.html)
- [Ruby Debugging Tips and Tricks](https://www.sitepoint.com/ruby-debugging-tips-tricks/)