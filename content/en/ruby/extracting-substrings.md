---
title:                "Extracting substrings"
html_title:           "Ruby recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Extracting substrings in Ruby refers to the process of retrieving a specific set of characters from a larger string. This is a common task in programming, as it allows for efficient manipulation and analysis of string data.

## How to:
To extract a substring in Ruby, we can use the built-in `slice` method. This method takes in two values, the starting index and the ending index, and returns the corresponding substring. For example:
```Ruby
string = "Hello world"
substring = string.slice(0, 5)
puts substring
```
Output:
```
Hello
```
If the ending index is omitted, the method will return all characters starting from the specified starting index. For instance:
```Ruby
string = "Goodbye world"
substring = string.slice(3..)
puts substring
```
Output:
```
dbye world
```
We can also use negative indexes to start counting from the end of the string. For example:
```Ruby
string = "Hello world"
substring = string.slice(-5..)
puts substring
```
Output:
```
world
```

## Deep Dive:
The `slice` method was introduced in Ruby version 1.8, and it is also aliased as `[]`. This method can also accept a range of indexes instead of just two values, allowing for even more flexibility in extracting substrings. Additionally, the `slice!` method can be used to delete the extracted substring from the original string. Alternatives to the `slice` method include using the `scan` method with a regular expression or using the `split` method with a delimiter.

## See Also:
- [Ruby Documentation on String Extraction](https://ruby-doc.org/core-2.7.1/String.html#method-i-slice)
- [Different Ways to Extract Substrings in Ruby](https://medium.com/@joelaguiar/different-ways-to-extract-substrings-in-ruby-47d56b3b2836)