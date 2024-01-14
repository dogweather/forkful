---
title:                "Ruby recipe: Searching and replacing text"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Have you ever found yourself needing to search and replace text in your code or document? Maybe you used a specific word or variable that you now want to change throughout your entire codebase. Or perhaps you accidentally mistyped a word and need to correct it everywhere. Whatever the reason may be, searching and replacing text is a common task in programming and can save you a lot of time and effort.

## How To

To search and replace text in Ruby, we can use the `gsub` method. This stands for "global substitution" and allows us to replace all instances of a specific string with a new string. Let's take a look at an example:

```Ruby
string = "Hello world!"
puts string.gsub("world", "Ruby")
```

The `gsub` method takes two arguments - the string we want to replace and the new string we want to replace it with. In this case, we are replacing the word "world" with "Ruby". The output of this code would be:

```
Hello Ruby!
```

If you want to replace all occurrences of a specific word regardless of its case, we can use the `gsub!` method with the `i` modifier:

```Ruby
string = "Hello World, hello WORLD!"
puts string.gsub!(/hello/i, "Hi")
```

In this example, we are using a regular expression with the `i` modifier to ignore case sensitivity. The output would be:

```
Hi World, Hi WORLD!
```

## Deep Dive

If we want to get more specific with our search and replace, we can use regular expressions. Regular expressions are sequences of characters that define a search pattern. Let's say we want to find and replace all numbers in a piece of text. We can use the `\d` pattern, which represents any digit, and the `+` modifier, which means one or more occurrences. Check out the following code:

```Ruby
string = "I have 5 apples and you have 2 oranges."
puts string.gsub(/\d+/, "10")
```

The output would be:

```
I have 10 apples and you have 10 oranges.
```

Regular expressions can get quite complex, so it's worth learning more about them and how to use them effectively for searching and replacing text.

## See Also

- [Ruby `gsub` documentation](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Regular Expressions in Ruby](https://www.regular-expressions.info/ruby.html)
- [Rubular](https://rubular.com/), an interactive regular expression tester for Ruby