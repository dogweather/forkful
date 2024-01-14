---
title:    "Ruby recipe: Deleting characters matching a pattern"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why
As programmers, we often encounter situations where we need to manipulate strings and remove unwanted characters. Deleting characters matching a pattern is a useful tool to have in our coding arsenal, as it allows us to quickly and efficiently clean up our data.

## How To
To delete characters matching a pattern in Ruby, we can use the `gsub` method. This method takes in two arguments: the pattern we want to match and the replacement string. Let's take a look at an example:

``` Ruby
sentence = "Hello, my name is John."
sentence = sentence.gsub(/[aeiou]/, "")
puts sentence
```
Output: Hll, my nm s Jhn.

In the above example, we are using a regular expression to match all the vowels in the `sentence` string and then replacing them with an empty string. This effectively deletes all the vowels from the string.

We can also use `gsub` with a block, to do more complex replacements. For example:

``` Ruby
phone_numbers = [
  "123-456-7890",
  "(555) 555-5555",
  "1-800-123-4567"
]

phone_numbers.each do |number|
  puts number.gsub(/\D/, "") { |match| match == "1" ? "+" : "" }
end
```
Output:
1234567890
+5555555555
+18001234567

In this example, we are removing all non-digit characters from the phone numbers and if the removed character is a "1", we are replacing it with a "+" symbol.

## Deep Dive
The `gsub` method stands for global substitution and is similar to the `sub` method, which only replaces the first match. By using `gsub`, we can ensure that all matches are replaced. Additionally, we can use various modifiers with `gsub` to make our pattern matching more specific. For example:

- `i` - case insensitive matching
- `m` - multiline pattern matching
- `x` - ignore whitespace and comments in the pattern

We can also use variables or other data structures as the replacement string, making the `gsub` method even more flexible.

## See Also
- [Ruby String documentation](https://ruby-doc.org/core-3.0.1/String.html)
- [Regular Expressions in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Ruby String Methods](https://www.rubyguides.com/2018/11/ruby-string-methods/)

Happy coding! =)