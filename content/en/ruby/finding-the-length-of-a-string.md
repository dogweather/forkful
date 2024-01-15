---
title:                "Finding the length of a string"
html_title:           "Ruby recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

You may be wondering why anyone would bother finding the length of a string in Ruby? Well, knowing the length of a string can be incredibly useful when working with text-based data. It allows us to manipulate and analyze strings in a more efficient and accurate way.

## How To

To find the length of a string in Ruby, we can use the `.length` method. Let's take a look at a simple example:

```Ruby
string = "Hello there!"
puts string.length
```

This code will output `12`, since there are 12 characters in the string "Hello there!" (including the space). It's important to note that the `.length` method will include all characters, including spaces, punctuation, and special characters.

We can also use the `.size` method, which essentially does the same thing as `.length` and is considered a synonym for it. So you can use whichever one you prefer.

Now, what if we want to find the length of a specific word within a string? For that, we can use the `.size` or `.length` method in combination with the `.split` method. Let's see an example:

```Ruby
string = "Ruby is a fun language!"
puts string.split(" ").last.length
```

This code will output `8`, since we are using the `.split` method to separate the string into an array of words and then accessing the last word (in this case, "language") and finding its length.

## Deep Dive

Behind the scenes, the `.length` or `.size` method in Ruby is actually calling the underlying `length` instance method. This method checks the `length` property of the string object and returns the number of characters. So, when we use `.length` or `.size`, we are just accessing this underlying method.

There is also a difference between the `length` and `size` methods when it comes to arrays. The `.size` method will return the number of items in the array, while the `.length` method will return the total number of elements in the multidimensional array. Keep this in mind when working with arrays in your code.

## See Also

- [Ruby String documentation](https://ruby-doc.org/core-3.0.2/String.html)
- [Ruby Arrays documentation](https://ruby-doc.org/core-3.0.2/Array.html)
- [Ruby language website](https://www.ruby-lang.org/en/)