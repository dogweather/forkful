---
title:                "Ruby recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to capitalize a string in your Ruby code but weren't sure how to do it? Look no further! In this blog post, we'll explore how to capitalize a string using Ruby programming.

## How To
To capitalize a string in Ruby, you can use the `capitalize` method. Let's see how this works with a simple example:

```Ruby
my_string = "hello world"
puts my_string.capitalize
```

The output for this code would be: `Hello world`. As you can see, the first letter of the string was capitalized.

You can also use the `capitalize!` method, which modifies the original string instead of creating a new one. Here's an example:

```Ruby
my_string = "hello world"
my_string.capitalize!
puts my_string
```

The output for this code would be: `Hello world`.

If you want to capitalize the first letter of each word in a string, you can use the `titleize` method. Let's try it out:

```Ruby
my_string = "hello world"
puts my_string.titleize
```

The output for this code would be: `Hello World`.

## Deep Dive
Behind the scenes, the `capitalize` method uses the `upcase` method to capitalize the first letter of the string and the `downcase` method to lowercase the remaining letters. This allows the method to work on both uppercase and lowercase strings.

You may also encounter the `swapcase` method, which reverses the case of each letter in a string. This can be useful if you need to change the case of a string, but not necessarily capitalize the first letter.

It's important to note that the `capitalize`, `upcase`, `downcase`, and `swapcase` methods only modify letters in the English alphabet. If your string contains special characters or non-English letters, the methods will not work as expected.

## See Also
For more information, check out the official Ruby documentation on string manipulation: https://ruby-doc.org/core-2.5.0/String.html

Other useful resources:
- Codecademy's Ruby course: https://www.codecademy.com/learn/learn-ruby
- Ruby Monk's String Basics tutorial: https://rubymonk.com/learning/books/4-ruby-primer-ascent/chapters/45-strings/lessons/108-string-basics