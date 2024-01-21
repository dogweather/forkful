---
title:                "Extracting substrings"
date:                  2024-01-20T17:46:34.472563-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracting substrings"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings is about pulling out specific bits of text from a string. Programmers do it to manipulate and use parts of data—like grabbing usernames from email addresses or parsing dates out of timestamps.

## How to:

Ruby makes extracting substrings simple. Let's cut to the chase:

```Ruby
str = "Hello, Ruby World!"

# Method 1: Using array indices
substring = str[7, 4] # "Ruby"
puts substring

# Method 2: Using the slice method
slice = str.slice(7, 4) # "Ruby"
puts slice

# Method 3: Regular expressions
match = str[/[Rr]uby/] # "Ruby"
puts match

# Method 4: split and array access
split_array = str.split # default splits on whitespace
picked_word = split_array[2] # "World!"
puts picked_word
```

Sample output for each snippet will be "Ruby", "Ruby", "Ruby", "World!" respectively.

## Deep Dive

Back in the day, extracting substrings was a more verbose process. Ruby's evolved, though. Today, you've got methods and regex at your disposal.

Here's what's happening under the hood: `[7, 4]` means start at the 7th character and grab the next 4. `slice` is just a methodic way to say the same thing. With regex, `/[Rr]uby/` is like saying, "Catch me a 'Ruby' or 'ruby’, whichever you find first." `split` chops the string into an array at each space, and `[2]` picks the third word—arrays start at zero, remember.

Alternatives? Sure, Ruby's got 'em. `partition`, `rpartition`, and `match` could also play here. Each has its case but knowing `.slice` and regex covers most bases. 

Bottom line: substring extraction is about precise text manipulation. The right tool means clean, effective code.

## See Also

- Ruby Docs on String: [ruby-doc.org/core-2.7.0/String.html](https://ruby-doc.org/core-2.7.0/String.html)
- Regular Expressions in Ruby: [ruby-doc.org/core-2.7.0/Regexp.html](https://ruby-doc.org/core-2.7.0/Regexp.html)
- Ruby Style Guide on Strings: [rubystyle.guide/#strings](https://rubystyle.guide/#strings)