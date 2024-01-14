---
title:                "Ruby recipe: Concatenating strings"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

In the world of programming, string manipulation is a common task that we encounter. Whether it's for displaying a message to the user or creating dynamic content, combining strings is a fundamental skill to have. In Ruby, there are various ways to concatenate strings, making it a versatile and useful tool for developers.

## How To

Concatenating strings in Ruby is a simple and straightforward process. The most commonly used method is the `+` operator, which allows us to join two or more strings together. Let's take a look at an example:

```Ruby
first_name = "John"
last_name = "Smith"
full_name = first_name + " " + last_name
puts full_name #=> "John Smith"
```

In this example, we declared two variables `first_name` and `last_name` that contain the corresponding strings. We then used the `+` operator to concatenate the strings and assigned it to a new variable `full_name`. Finally, we printed the `full_name` variable to the console, which outputs the concatenated string "John Smith".

Another way to concatenate strings in Ruby is by using the `<<` operator, which is also known as the concatenation assignment operator. This operator modifies the string directly, making it a more efficient approach. Let's see an example:

```Ruby
greeting = "Hello"
greeting << " "
greeting << "World"
puts greeting #=> "Hello World"
```

In this example, we started with the `greeting` variable containing the string "Hello". Then, we used the `<<` operator to append the space and the word "World" to the string, resulting in the output "Hello World".

There are also other methods that can be used for concatenating strings, such as the `concat()` and `prepend()` methods. It's important to note that the best method to use for concatenating strings depends on the specific task at hand.

## Deep Dive

Behind the scenes, concatenating strings in Ruby involves creating a new string object that contains the combined strings. This is why it's important to be mindful of memory usage when concatenating large strings. 

In addition, it's worth mentioning that Ruby's `+` operator is actually an alias for the `concat()` method. This means that using the `+` operator behind the scenes calls the `concat()` method, making it a matter of personal preference as to which method to use. 

In contrast, the `<<` operator modifies the existing string object directly, making it a more efficient approach in terms of memory usage. However, it's important to note that it can have unintended consequences if used incorrectly, such as altering the original string unintentionally.

## See Also

- [Ruby String Concatenation](https://www.rubyguides.com/ruby-tutorial/ruby-string-concatenation/)
- [Ruby String Concatenation | Ruby Tutorial](https://www.youtube.com/watch?v=m1w4HjMXUbE)
- [Understanding string concatenation in Ruby](https://dev.to/fakorede/understanding-string-concatenation-in-ruby-4kdn)