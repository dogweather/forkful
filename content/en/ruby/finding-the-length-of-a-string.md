---
title:                "Finding the length of a string"
date:                  2024-01-20T17:48:07.158280-07:00
model:                 gpt-4-1106-preview
simple_title:         "Finding the length of a string"

category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Finding the length of a string means counting its characters. It's basic but crucial for tasks like validation, text-processing, and determining storage needs.

## How to:
Ruby keeps it simple with the `.length` method:

```ruby
greeting = "Hello, world!"
puts greeting.length
```

Output:

```
13
```

Or, use `.size` which does the same thing:

```ruby
greeting = "Hello, world!"
puts greeting.size
```

Output:

```
13
```

## Deep Dive
In Ruby, `.length` and `.size` are interchangeable when it comes to strings; they give you the character count. Historically, Ruby has focused on making the code more natural to read, which is why you often find more than one way to do the same thing.

Internally, each character in a string affects the storage size. So, knowing the number can be essential for optimization, especially with massive amounts of text.

While `.length` and `.size` give you the character count, in some languages and earlier times, a string's length might refer to its byte size. Ruby with its multibyte character support via Unicode, however, does not equate byte size directly to string length due to characters possibly taking more than one byte.

Alternatives like `.bytesize` tell you how many bytes a string takes up, and `.chars.count` gives you the number of characters by first converting the string into an array of characters.

Here's how you would use `.bytesize` and `.chars.count`:

```ruby
greeting = "Hello, world!"
puts greeting.bytesize
puts greeting.chars.count
```

Output:

```
13
13
```

## See Also
- Ruby documentation on Strings: [https://ruby-doc.org/core/String.html](https://ruby-doc.org/core/String.html)
- A nice primer on Ruby Strings by [RubyGuides](https://www.rubyguides.com/2018/01/ruby-string-methods/): explore more on what you can do with strings beyond measuring their size.
- Dive into character encoding and how it affects string operations with [this article from Thoughtbot](https://thoughtbot.com/blog/its-about-time-zones#character-encoding).
