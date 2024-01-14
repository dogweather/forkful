---
title:    "Ruby recipe: Finding the length of a string"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

As a programming language, Ruby offers a wide range of built-in methods that allow developers to manipulate and analyze strings. One such method is `length`, which is used to find the length of a string. This may seem like a simple task, but it can be incredibly useful in various programming scenarios. Keep reading to learn more about why you should consider using this method in your Ruby projects.

## How To

Finding the length of a string in Ruby is easy and straightforward. You can simply call the `length` method on a string object and it will return the number of characters in that string. Let's take a look at an example:

```Ruby
my_string = "Hello, world!"
puts my_string.length
```

The above code will output `13`, which is the length of the string "Hello, world!". Additionally, you can also use the `size` method, which is an alias of `length` and performs the same task. Here's an example of using `size`:

```Ruby
my_string = "Goodbye"
puts my_string.size
```

This code will output `7`, which is the length of the string "Goodbye". As you can see, the `length` method can be used to find the length of any string, regardless of its contents.

## Deep Dive

Under the hood, the `length` method works by counting the number of characters in a string. This includes letters, numbers, spaces, symbols, and even special characters. It also counts the white space characters, such as tabs and newlines. This is why the `length` of a string does not always correspond to the number of visible characters in that string.

It's important to note that the `length` method is only available for string objects. If you try to use it on any other type of object, you will get an error. Additionally, the `length` method is not limited to strings with a fixed length. It can also be used with string variables and even strings that are created dynamically at runtime.

## See Also

To learn more about the `length` method and how to use it effectively in your Ruby projects, check out the official Ruby documentation and these helpful resources:

- [Ruby `length` Method](https://ruby-doc.org/core-2.7.2/String.html#method-i-length)
- [Ruby String Methods Guide](https://www.rubyguides.com/2019/07/ruby-string-methods/)
- [Ruby String Tutorial](https://www.rubyguides.com/2015/06/ruby-string/)