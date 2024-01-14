---
title:    "Ruby recipe: Extracting substrings"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to extract a specific part of a string in your Ruby program? Maybe you wanted to get just the name from a full name email address. Or perhaps you needed to extract the digits from a phone number. Whatever the case may be, being able to extract substrings is a useful skill in Ruby programming. In this blog post, we'll take a look at why it's important to know how to extract substrings and how you can do it using Ruby.

## How To

The `String` class in Ruby comes with a handy method, `slice`, that allows us to extract a substring from a string based on specific starting and ending positions. Let's look at an example:

```ruby
name = "John Doe"
puts name.slice(0, 4)
```

This code will output "John", as it starts at index 0 and extracts 4 characters. We can also use negative indexes to start from the end of the string. For instance, `name.slice(-3, 3)` would output "Doe" since it starts 3 characters from the end and extracts 3 characters.

But what if we wanted to extract a substring based on a certain pattern? That's where regular expressions come in. We can use the `match` method to search for a specific pattern in a string and extract the matching substring. Take a look at this example:

```ruby
phone_number = "555-123-4567"
puts phone_number.match(/\d{3}-\d{3}-\d{4}/)
```

This code will output "555-123-4567" since it matches the pattern of three digits, a hyphen, three digits, a hyphen, and four digits.

Lastly, we can also use `split` to extract substrings from a string based on a delimiter. For example:

```ruby
address = "123 Main St, New York, NY"
puts address.split(",")[0]
```

This code will output "123 Main St" by splitting the string at each comma and returning the first element.

## Deep Dive

Aside from the methods mentioned above, there are other ways to extract substrings in Ruby such as using the `[]` operator or the `scan` method for multiple matches. Additionally, there are also several string manipulation libraries available that offer more advanced substring extraction options.

It's important to keep in mind that different methods will be more suitable for different scenarios. For example, using regular expressions may be more efficient for extracting substrings based on specific patterns, while using `slice` would be better for extracting a substring at a specific position.

## See Also

To learn more about extracting substrings in Ruby, check out these helpful resources:

- [Ruby String Documentation](https://ruby-doc.org/core-2.6/String.html)
- [Regular Expressions in Ruby](https://www.rubyguides.com/2015/06/ruby-regexp/)
- [String Manipulation Libraries in Ruby](https://www.ruby-toolbox.com/categories/string_manipulation_libraries)