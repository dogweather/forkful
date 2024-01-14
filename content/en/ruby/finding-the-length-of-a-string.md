---
title:                "Ruby recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever wondered how computer programs are able to determine the length of a string of text? Or why this seemingly simple task is necessary in programming? As it turns out, finding the length of a string is a fundamental skill in computer programming and can be very useful in various situations. In this blog post, we will explore why and how to find the length of a string using the Ruby programming language.

## How To

To find the length of a string in Ruby, we can use the `length` method. Let's take a look at a simple example:

```Ruby
str = "Hello, world!"
puts str.length

# Output: 13
```

In the example above, we first declare a variable `str` and assign it a string value of "Hello, world!" We then use the `length` method to determine the length of the string and print it out. The output we get is 13 because the string contains 13 characters, including the space and exclamation mark.

We can also use the `length` method on user input. Let's say we want to create a program that counts the characters in a user's name. We can do so using this code:

```Ruby
puts "Please enter your name:"
name = gets.chomp
puts "Your name has #{name.length} characters."
```

In the example above, we first prompt the user to enter their name and store it in a variable `name`. Then, we use the `length` method to determine the length of the string and print it out along with a message. Give it a try by running this code in your own Ruby environment!

## Deep Dive

Under the surface, the `length` method is actually calling the `size` method, which in turn uses the `length` method of the underlying data structure of the string. This is just one of the many examples of the convenience and efficiency of Ruby's built-in methods. Additionally, the `length` method can be used on different data types, such as arrays and hashes, to determine the number of items in them.

It is worth noting that the `length` method gives the number of characters in a string, not the number of words. To count the number of words, we can use the `split` method to split the string into an array of words and then use the `length` method on the resulting array.

## See Also
- [Ruby String Documentation](https://ruby-doc.org/core/String.html#method-i-length)
- [Ruby Array Documentation](https://ruby-doc.org/core/Array.html#method-i-length)
- [Ruby Hash Documentation](https://ruby-doc.org/core/Hash.html#method-i-length)