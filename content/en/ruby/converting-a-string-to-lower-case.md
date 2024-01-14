---
title:                "Ruby recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

As a Ruby programmer, you may come across situations where you need to convert a string to lower case. This simple task can be useful when you want to ensure consistency or make string comparisons case-insensitive.

## How To

To convert a string to lower case in Ruby, you can use the `downcase` method. Here's an example:

```Ruby
string = "HELLO WORLD"
puts string.downcase
```

This code will output `hello world` to the console. As you can see, the `downcase` method converts all characters in the string to their lower case equivalent. This method does not modify the original string, but returns a new string with the changes applied.

You can also use the `downcase!` method to modify the original string itself. Here's an example:

```Ruby
string = "I LOVE RUBY"
string.downcase!
puts string
```

This will output `i love ruby` to the console. The `downcase!` method modifies the string directly, which can be useful if you don't need the original string anymore.

## Deep Dive

So how does the `downcase` method work? Behind the scenes, Ruby utilizes Unicode case mappings to convert the string characters. This means that the method is not limited to the English alphabet, but can handle international characters as well.

In addition, the `downcase` method also takes into account the current locale of the system. This means that the results may vary depending on the language and cultural conventions of the system where the code is executed.

It's also worth noting that the `downcase` method only converts uppercase characters to their lowercase equivalent. Any non-alphabetic characters, such as numbers or symbols, will not be affected. If you want to also convert these characters to lowercase, you can combine the `downcase` method with the `gsub` method.

## See Also

Here are some helpful links for further reading on string manipulation in Ruby:

- [Ruby String Documentation](https://ruby-doc.org/core-3.0.2/String.html)
- [Unicode Case Mappings](https://unicode.org/faq/casemap_charprop.html)
- [Locale-Sensitive Case Mapping in Ruby](https://www.rubydoc.info/github/ruby/ruby/Rake%2FLocale/CONVICTION)

Happy coding!