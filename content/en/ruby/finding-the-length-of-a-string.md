---
title:                "Ruby recipe: Finding the length of a string"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

As a beginner in Ruby programming, it can be overwhelming to learn all the different concepts and methods. However, one basic task that you will frequently encounter when working with strings is finding the length of a string. This may seem like a simple task, but understanding how to properly find the length of a string is an essential building block in becoming proficient in Ruby.

## How To

To find the length of a string in Ruby, you can use the `.length` or `.size` method. Let's take a look at an example:

```Ruby
my_string = "Hello World"
puts my_string.length
```

The output of this code will be `11`, which is the number of characters in the string including the space between "Hello" and "World". Similarly, you can also use the `.size` method to get the same result.

```Ruby
my_string = "Hello World"
puts my_string.size
```

Another way to find the length of a string is by using the `size` method with the `to_a` method. This will convert the string into an array of characters, and then you can use the `size` method on the array to find the length.

```Ruby
my_string = "Hello World"
puts my_string.to_a.size
```

## Deep Dive

Now that we have covered the basics of finding the length of a string, let's dive deeper into the inner workings of these methods.

The `.length` and `.size` methods are both built-in methods for strings in Ruby, which means you do not need to require any additional libraries to use them. These methods work by counting the number of characters in the string, including spaces and special characters. Therefore, the return value will always be an integer.

It's important to note that these methods work on Unicode strings, which means that each character in the string is counted equally regardless of the language or symbol used.

## See Also

To learn more about working with strings in Ruby, check out the following resources:

- [Ruby String Documentation](https://ruby-doc.org/core-2.7.1/String.html)
- [Ruby Strings Tutorial on Codecademy](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-string-methods)
- [Ruby String Basics on Ruby Guides](https://www.rubyguides.com/2018/07/ruby-string-basics/)

Now that you have a better understanding of how to find the length of a string in Ruby, go ahead and practice using these methods in your own code. Keep learning and exploring the world of Ruby!