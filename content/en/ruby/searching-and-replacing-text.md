---
title:    "Ruby recipe: Searching and replacing text"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Why

When working with large amounts of text in a program, it is often necessary to replace certain words or phrases with something else. This can save time and effort by avoiding the need to manually edit each instance of the text. Thankfully, Ruby has built-in methods for searching and replacing text, making this task much easier.

## How To

In Ruby, the `gsub` method can be used to perform a search and replace operation on a string. It takes two arguments: the target string to be replaced, and the replacement string. Let's look at an example:

```Ruby
text = "I love programming"
new_text = text.gsub("programming", "coding")
puts new_text
```

The output of this code will be:

```
I love coding
```

As you can see, the word "programming" has been replaced with "coding" using the `gsub` method.

Multiple words can also be replaced at once by using a regular expression instead of a string as the first argument. For example:

```Ruby
text = "Programming is my favorite hobby"
new_text = text.gsub(/programming|hobby/, "coding")
puts new_text
```

This will output:

```
Coding is my favorite coding
```

The pipe symbol `|` is used to separate multiple words in the regular expression.

## Deep Dive

Behind the scenes, the `gsub` method works by using regular expressions to match and replace patterns of characters in a string. This allows for more flexibility in the search and replace process. For example, instead of just replacing a specific word, you can replace any word that starts with a certain letter or combination of letters.

Additionally, the `gsub` method has a bang counterpart which modifies the original string instead of creating a new one. This can be useful if you want to make changes to a string in place without creating a new variable.

## See Also

- [Ruby documentation on `gsub`](https://ruby-doc.org/core-2.7.2/String.html#method-i-gsub)
- [Regular Expressions in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Using `gsub` for advanced text manipulation in Ruby](https://airbrake.io/blog/ruby/gsub-method)