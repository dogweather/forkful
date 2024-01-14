---
title:                "Ruby recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

In any programming language, string manipulation is a crucial skill to have. Whether you're building a website, creating a game, or simply trying to organize data, being able to combine strings together can make your life a whole lot easier. In this blog post, we'll be focusing specifically on concatenating strings in Ruby - a simple yet powerful concept that every Ruby programmer should know.

## How To

Concatenating strings in Ruby is as simple as using the `+` operator. Let's take a look at a basic example:

```Ruby
name = "John"
greeting = "Hello"

puts greeting + " " + name
# Output: Hello John
```

In this example, we have two separate strings - `name` and `greeting` - and we combine them using the `+` operator. We also add a space in between the two strings to make the output more readable.

You can also use the `concat` method in Ruby to concatenate strings. Here's an example:

```Ruby
message = "I love"
message.concat(" Ruby!")

puts message
# Output: I love Ruby!
```

As you can see, the `concat` method modifies the original string and adds the specified string to the end.

Concatenation can also be done with variables that are not just strings. For example:

```Ruby
age = 25
message = "I am " + age.to_s + " years old."

puts message
# Output: I am 25 years old.
```

In this case, we converted the `age` variable to a string using the `to_s` method before concatenating it with the other strings.

## Deep Dive

Under the hood, concatenating strings in Ruby involves creating a new string that is a combination of two or more existing strings. This can be seen in the examples above where we used the `+` operator or the `concat` method. However, it's important to note that strings in Ruby are immutable, meaning they cannot be changed once they are created. This is why when we use concatenation, a new string is created instead of changing the original ones.

It's also worth mentioning that there is a more efficient way of concatenating strings in Ruby - using the `<<` operator. Let's take a look at an example:

```Ruby
name = "John"
greeting = "Hello"

greeting << " " << name

puts greeting
# Output: Hello John
```

In this case, the original `greeting` string is modified instead of creating a new one. This makes the `<<` operator a better choice for concatenation when performance is a concern.

## See Also

Here are some additional resources to help you learn more about concatenating strings in Ruby:

- [Ruby Documentation on String Concatenation](https://ruby-doc.org/core-3.0.2/String.html#method-i-2B)
- [RubyMonk Tutorial on String Concatenation](https://rubymonk.com/learning/books/1-ruby-primer/chapters/3-strings/lessons/22-string-concatenation)
- [RubyGuides Article on String Concatenation](https://www.rubyguides.com/2019/10/ruby-string-concatenation/)

Happy coding!