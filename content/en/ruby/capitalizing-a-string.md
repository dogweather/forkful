---
title:    "Ruby recipe: Capitalizing a string"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why 

Capitalizing a string may seem like a simple task, but it can have a significant impact on the readability and professionalism of your code. This seemingly small detail can make a big difference in the overall quality of your program.

## How To

To capitalize a string in Ruby, we can use the `upcase` method. This method converts all lowercase letters in a string to uppercase, leaving any uppercase letters unchanged.

```
Ruby String Example:

name = "jennifer"
puts name.upcase

Output:
JENNIFER
```

We can also use the `capitalize` method to only capitalize the first letter of a string, leaving the rest unchanged.

```
Ruby String Example:

name = "john"
puts name.capitalize

Output:
John
```

It's important to remember that these methods will not alter the original string, but instead return a new string with the desired capitalization.

## Deep Dive

Behind the scenes, the `upcase` and `capitalize` methods use the `Unicode` standard to determine which characters should be converted to uppercase. This standard covers a vast range of characters from various alphabets, symbols, and even emojis.

It's also worth noting that these methods only work on individual characters within a string, not entire words or phrases. If we want to capitalize every word in a string, we can use the `titleize` method from the `titleize` string extension library.

```
Ruby String Example:

require 'titleize'

sentence = "hello world"
puts sentence.titleize

Output:
Hello World
```

## See Also

- [Ruby String Documentation](https://ruby-doc.org/core-2.7.2/String.html)
- [Unicode Standard](https://www.unicode.org/)
- [Titleize String Extension Library](https://www.rubydoc.info/gems/titleize/0.0.3)