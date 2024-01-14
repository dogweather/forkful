---
title:                "Ruby recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging plays a crucial role in the process of programming. It allows developers to identify and fix errors in their code, ensuring that the program runs smoothly and produces the expected results. One useful tool for debugging is printing debug output, also known as "console logging". In this blog post, we will explore why printing debug output is important and how it can help us in our coding journey.

## How To

To print debug output in Ruby, we can use the built-in `puts` method. This method takes in a string as an argument and displays it on the console. Let's take a look at an example:

```Ruby
str = "Debugging is important!"
puts str
```

The above code will output `Debugging is important!` on the console. We can also use the `p` method, which not only prints the string but also includes the quotation marks and escape characters. Here's an example:

```Ruby
str = "Debugging is important!"
p str
```

The output will be `"Debugging is important!"`. This can be helpful when debugging code that involves special characters or formatting.

Another useful method for printing debug output is `pp`, which stands for "pretty print". This method displays objects in a more readable and organized format. For example:

```Ruby
arr = [1, 2, 3]
pp arr
```

The output will be displayed as:

```Ruby
[1, 2, 3]
```

This can be especially helpful when working with complex data structures.

## Deep Dive

Printing debug output is not just useful for displaying values and variables. It can also help us track the flow of our code and identify any potential errors. By strategically placing `puts` statements throughout our code, we can see which parts of the code are being executed and in what order. This can be particularly helpful when dealing with loops or conditional statements.

Furthermore, printing debug output also allows us to see the actual values of the variables at different points in our code. This can help us pinpoint any unexpected changes or errors that may be occurring.

It's important to note that while printing debug output can be helpful, it should not be relied upon as the sole method of debugging. It's always important to understand the logic behind our code and thoroughly test it to ensure its functionality.

## See Also

For further reading on debugging and printing debug output in Ruby, check out these resources:

- [https://ruby-doc.org/core-2.5.1/Kernel.html#method-i-pp](https://ruby-doc.org/core-2.5.1/Kernel.html#method-i-pp)

- [https://www.rubyguides.com/2019/02/ruby-debugging/](https://www.rubyguides.com/2019/02/ruby-debugging/)

- [https://www.sitepoint.com/debugging-ruby-code-beginner/](https://www.sitepoint.com/debugging-ruby-code-beginner/)