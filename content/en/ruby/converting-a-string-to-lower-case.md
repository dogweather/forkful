---
title:    "Ruby recipe: Converting a string to lower case"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why
Have you ever encountered a situation where you needed to compare two strings, but one was in all lowercase and the other was in uppercase? This can make it difficult to accurately match the strings. That's where converting a string to lower case comes in handy!

## How To
Converting a string to lower case in Ruby is simple. Just use the `downcase` method on the string variable. Let's take a look at an example:

```ruby
string = "HELLO WORLD"
puts string.downcase
```
Output: `hello world`

As you can see, the `downcase` method converts all letters in the string to their lowercase counterparts. This is especially useful when you want to compare strings without worrying about case sensitivity.

## Deep Dive
It's important to note that the `downcase` method only works on letters that have lowercase equivalents. So, any punctuation or special characters will not be converted. Let's take a look at another example:

```ruby
string = "Hello, World!"
puts string.downcase
```
Output: `hello, world!`

Notice how the exclamation point remained in uppercase. Additionally, the `downcase` method will also not change the case of numbers. So if you have numbers in your string, they will remain the same.

Another thing to keep in mind is that the `downcase` method is not permanent. It will only convert the string temporarily. If you want to permanently change the case of a string, you can use the `downcase!` method, which will modify the original string.

## See Also
If you want to learn more about string manipulation in Ruby, check out these additional resources:

- [Ruby String Documentation](https://ruby-doc.org/core-2.7.0/String.html)
- [Ruby String Methods](https://ruby-doc.org/core-2.7.0/String.html#method-i-downcase)
- [Ruby String Operations](https://www.rubyguides.com/2015/05/working-with-strings-in-ruby/)