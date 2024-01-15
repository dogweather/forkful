---
title:                "Converting a string to lower case"
html_title:           "Ruby recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case may seem like a simple task, but it can actually have a big impact on the readability and functionality of your code. By converting a string to lower case, you are making it easier to compare and manipulate strings, which can help improve the overall functionality and efficiency of your code.

## How To

```Ruby
# To convert a string to lower case, use the `downcase` method:
string = "HELLO WORLD"

puts string.downcase
# Output: hello world
```

```Ruby
# You can also use the `downcase!` method to modify the original string:
string = "HELLO WORLD"

string.downcase!

puts string
# Output: hello world
```

```Ruby
# It's important to note that the `downcase` and `downcase!` methods only work on strings, not on other data types like integers or floats.
# If you try to use these methods on a non-string object, you will get an error message.
```

## Deep Dive

Converting a string to lower case may seem like a simple task, but there are a few key things to keep in mind. Firstly, the `downcase` method will create a new string object with the lower case letters, while the `downcase!` method will modify the original string in place. This can be important when working with immutable strings or if you want to preserve the original string for later use.

Additionally, the `downcase` and `downcase!` methods use the rules of the English alphabet. This means that any special characters or letters with accent marks will not be properly converted. To account for this, you can use the `unicode_normalize` method before using the `downcase` and `downcase!` methods.

## See Also

- [String Documentation - Ruby](https://ruby-doc.org/core-2.7.2/String.html)
- [Case Conversion Methods - Ruby](https://ruby-doc.org/core-2.7.2/String.html#method-i-downcase)