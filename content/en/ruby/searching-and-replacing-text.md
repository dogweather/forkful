---
title:                "Ruby recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Text processing is an essential task in programming, and being able to efficiently search and replace text can save you a lot of time and effort. Whether it's correcting mistakes, updating outdated information, or changing formatting, having the ability to replace text in your code can make your life as a programmer much easier.

## How To
Searching and replacing text in Ruby is made simple with the `.gsub` method. This method allows you to search for a specific string and replace it with another string. Let's take a look at an example:

```Ruby
sentence = "I love programming in Ruby!"

new_sentence = sentence.gsub("Ruby", "Python")

puts new_sentence #=> "I love programming in Python!"
```

In the above code, we are using the `.gsub` method to search for the word "Ruby" in the `sentence` variable and replace it with the word "Python". The new sentence is then stored in the `new_sentence` variable and printed using the `puts` method.

You can also use regular expressions with the `.gsub` method to make more complex replacements. Let's see an example:

```Ruby
sentence = "I love practicing yoga and meditation."

new_sentence = sentence.gsub(/(yoga|meditation)/, "hiking")

puts new_sentence #=> "I love practicing hiking and hiking."
```

In this example, we are using a regular expression to search for either "yoga" or "meditation" and replace them with the word "hiking". By using parentheses, we can specify multiple words that we want to replace with the same string.

## Deep Dive
The `.gsub` method also has additional options that allow you to make more precise replacements. For example, you can use the `count` argument to specify the maximum number of replacements to be made. Let's take a look at an example:

```Ruby
sentence = "I love Ruby, Ruby, Ruby!"

new_sentence = sentence.gsub("Ruby", "Python", 2)

puts new_sentence #=> "I love Python, Python, Ruby!"
```

In this example, we are using the `count` argument to limit the number of replacements to only 2. This means that only the first two instances of "Ruby" will be replaced with "Python", and the third one will remain unchanged.

## See Also
- [Ruby documentation on .gsub method](https://ruby-doc.org/core-2.7.2/String.html#method-i-gsub)
- [Regular expressions in Ruby](https://ruby-doc.org/core-2.7.2/Regexp.html)
- [Rubular - a Ruby regular expression editor](https://rubular.com/)