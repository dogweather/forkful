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

## Why
Do you ever find yourself needing to extract a specific piece of text from a larger string? Perhaps you need to separate a name from an email address or isolate a date from a paragraph of text. This is where extracting substrings becomes incredibly useful.

## How To
To extract substrings in Ruby, we can use the `[]` method. Let's look at a few examples to see how this works.

**Extracting characters:** If we have a string `name = "John Doe"`, we can use `name[0]` to extract the first character, which would output "J". Similarly, `name[5]` would output "D".

**Extracting ranges:** We can also use ranges to extract a specific section of a string. For example, `name[0..3]` would output "John", and `name[5..6]` would output "Do".

**Extracting with index and length:** If we need to extract a specific number of characters from a certain point in the string, we can use the `slice` method. For instance, `name.slice(5, 2)` would output "Do", since it starts at index 5 and takes 2 characters.

**Sample Output:**
```
name = "John Doe"
name[0] # "J"
name[5] # "D"
name[0..3] # "John"
name[5..6] # "Do"
name.slice(5, 2) # "Do"
```

## Deep Dive
Now that we know the basics of extracting substrings, let's dive a bit deeper into the `[]` method. We can use this method to not only get single characters or ranges, but also to perform more complex operations.

**Using patterns:** We can use regular expression patterns within the `[]` method to extract specific patterns from a string. For example, `name[/[a-z]+/]` would output "ohn" as it matches any lowercase characters.

**Nested `[]` methods:** We can also nest `[]` methods within each other to extract nested substrings. For instance, `name[1..-1][2..-1]` would output "hn Do" since it first extracts "ohn Doe", and then takes the substring from index 2 to the end.

**Using `capitalize`:** Lastly, we can use the `capitalize` method within `[]` to extract substrings with the first character capitalized. For example, `name[5..-1].capitalize` would output "Doe".

## See Also
For more information on extracting substrings in Ruby, check out these resources:

- [Ruby String Documentation](https://ruby-doc.org/core-2.6.3/String.html#method-i-5B-5D)
- [Ruby Regexp Documentation](https://ruby-doc.org/core-2.6.3/Regexp.html)
- [The `capitalize` Method in Ruby](https://www.rubyguides.com/2018/05/string-capitalize-method/)