---
title:    "Ruby recipe: Printing debug output"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

As a programmer, you may have encountered situations where your code is not functioning as expected. When this happens, it can be frustrating to figure out where the issue lies. This is where printing debug output becomes useful. By strategically placing print statements in your code, you can get a better understanding of the program's flow and identify any errors that may be causing the unexpected behavior.

## How To

To print debug output in your Ruby program, you can use the `print` or `puts` methods. These methods allow you to print out values or messages to the console during runtime. Let's look at an example:

```Ruby
def multiply(num1, num2)
    print "Multiplying #{num1} and #{num2}..."
    product = num1 * num2
    puts "The result is #{product}."
    return product
end

multiply(5, 7)
# Output:
# Multiplying 5 and 7...
# The result is 35.
```

In the above code, we have added print statements to show us the values of the two numbers being multiplied and the final result. This can give us a better understanding of how the function is working and help us identify any errors.

Another useful method for debugging is `p`, which stands for "print" and allows you to print out the value of an object or variable. It also displays the data type of the value, which can be helpful for debugging. Let's modify our previous example to use `p` instead of `print`:

```Ruby
def multiply(num1, num2)
    p "Multiplying #{num1} and #{num2}..."
    product = num1 * num2
    p product
    return product
end

multiply(5, 7)
# Output:
# "Multiplying 5 and 7..."
# 35
```

Now we can see that the `product` variable is an integer with a value of 35.

## Deep Dive

While using print statements for debugging can provide a quick solution, it is not the most efficient method. Ruby has a built-in `debugger` library that allows you to pause the execution of your code at a specific point and interactively inspect the program's state. This can be especially useful for debugging complex issues.

To use the `debugger` library, you first need to require it in your code:

```Ruby
require 'debugger'
```

Next, you can insert the `debugger` method at any point in your code where you want it to pause:

```Ruby
def multiply(num1, num2)
    product = num1 * num2
    debugger
    return product
end

multiply(5, 7)
```

When the program reaches the `debugger` method, it will pause the execution and give you an interactive shell where you can inspect the values of variables and objects.

To exit the debugger, you can type `c` for "continue" and press enter.

## See Also

- [Debugging with the `debugger` library](https://ruby-doc.org/stdlib-2.7.1/libdoc/debug/rdoc/index.html)
- [Using `p` for debugging](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-p)
- [Using `print` and `puts` for debugging](https://ruby-doc.org/core-2.7.1/IO.html#method-i-print)

Debugging is an essential skill for any programmer, and using print statements and the `debugger` library can significantly improve your debugging process in Ruby. So next time you encounter an error in your code, don't forget to print out some debug output to help you troubleshoot.