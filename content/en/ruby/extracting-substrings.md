---
title:                "Ruby recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why 
Have you ever needed to extract a specific part of a string in your Ruby program? Maybe you only want the first few characters, or you want to remove certain characters from the string. That's where the `substring` method comes in handy. By extracting substrings, you can manipulate strings in various ways to fit your needs.

## How To
The `substring` method allows you to extract a specified portion of a string by providing a starting index and an optional ending index. Let's take a look at a simple example:

```ruby
string = "Hello World"
puts string.substring(0,5) # Output: "Hello"
```

In this example, we start at index 0 and extract 5 characters, which gives us the first word "Hello". We can also use negative numbers for the index, where -1 represents the last character in the string. For example:

```ruby
puts string.substring(6,-1) # Output: "World"
```

We can also omit the ending index and it will automatically extract the remaining characters from the starting index to the end of the string. For instance:

```ruby
puts string.substring(3) # Output: "lo World"
```

We can even use the `substring` method on arrays of strings. Let's say we have an array of names and we only want to extract their first names. We can use the `map` method to go through each element in the array and extract the first name using the `substring` method:

```ruby
names = ["John Smith", "Anna Jones", "Mike Brown"]

first_names = names.map { |name| name.substring(0, name.index(' ')) }

puts first_names # Output: ["John", "Anna", "Mike"]
```

## Deep Dive
The `substring` method uses one-based indexing, meaning the first character in the string is at index 1, not 0. This can be confusing at first, especially if you're used to zero-based indexing in other programming languages.

Another thing to keep in mind is that the ending index is exclusive. This means the character at the ending index will not be included in the extracted substring. For example, using `substring(0,5)` will extract 5 characters starting from index 0, but the character at index 5 will not be included.

It's also worth noting that the `substring` method does not modify the original string, instead it returns a new substring. So if you want to save the extracted substring, make sure to assign it to a variable like we did in the examples above.

## See Also
- [Ruby Documentation on Substrings](https://ruby-doc.org/core-2.4.0/String.html#method-i-substring)
- [Ruby String Methods](https://www.rubyguides.com/2019/04/ruby-string-methods/)

Now that you know how to use the `substring` method, you can manipulate strings in even more creative ways. Happy coding!