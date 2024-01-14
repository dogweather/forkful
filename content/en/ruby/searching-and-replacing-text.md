---
title:    "Ruby recipe: Searching and replacing text"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Searching and replacing text is a common task in programming, especially when dealing with large amounts of data. It allows you to efficiently make bulk changes to your code or text without having to manually go through each occurrence. In this blog post, we will explore how to do this in Ruby, a popular and versatile programming language.

## How To
First, let's create a string with some sample text as an example:

```Ruby
str = "I love programming in Ruby!"
```

To replace a specific word or phrase in a string, we can use the `gsub` method. This method takes two arguments: the string to be replaced and the replacement string.

```Ruby
str.gsub("love", "enjoy")
```

This will output: "I enjoy programming in Ruby!"

We can also use regular expressions to search and replace patterns in a string. For example, if we want to remove all numbers from a string, we can use the `\d` regex pattern which represents any digit:

```Ruby
str = "There are only 3 days left before the deadline!"
str.gsub(/\d/, "")
```

This will output: "There are only days left before the deadline!"

Additionally, we can use the `sub` method to only replace the first occurrence of a string, instead of all occurrences. This can be useful in certain situations where you only want to modify a specific part of a string.

```Ruby
str = "Ruby is the best language! Ruby is my favorite!"
str.sub("Ruby", "Python")
```

This will output: "Python is the best language! Ruby is my favorite!"

## Deep Dive
The `gsub` and `sub` methods are just a few of the many ways you can search and replace text in Ruby. There are also other methods such as `tr` and `tr_s` that allow you to replace characters in a string with other characters or delete them entirely. Additionally, Ruby has powerful regular expression capabilities that make searching and replacing even more robust.

It's important to note that these methods do not modify the original string, but rather return a new string with the modifications. If you want to modify the original string, you can use the destructive versions of these methods, `gsub!` and `sub!`.

## See Also
- Ruby String Documentation: https://ruby-doc.org/core-3.0.2/String.html
- Ruby Regular Expressions Tutorial: https://www.rubyguides.com/2015/06/ruby-regex/
- Ruby Official Website: https://www.ruby-lang.org/en/