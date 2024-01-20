---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings is the act of pulling out smaller strings from a larger one using specific indexes. Programmers do this to manipulate, analyze, or segment relevant parts of data within larger text bodies.

## How to:

Ruby provides several methods to extract substrings, let's dive into some.

```Ruby
str = 'WelcometoRubyProgramming'

# Using slice method
puts str.slice(8, 4)  # output: "toRu"

# Using [] method
puts str[8..11]  # output: "toRu"

# Using slice() with a range
puts str.slice(8..11)  # output: "toRu"
```

Note: Ruby counts from zero, so the 8th character is 't'.

## Deep Dive

Extracting substrings has long been a fundamental aspect of programming, tracing back to when manipulation of raw text data became viable. Besides the methods shown, Ruby offers `.substring()`, which behaves similarly. 

Alternatives depend on necessity. Regular expressions could be used to match patterns instead of direct indices. 

When picking a method, consider usage context. `.slice()` modifies the original string and could be risky. `.substring()` doesn't alter the source but requires more memory as it generates a new string. Choose wisely!

## See Also

1. Ruby Doc String slice : [https://ruby-doc.org/core-2.7.0/String.html#method-i-slice](https://ruby-doc.org/core-2.7.0/String.html#method-i-slice)
2. Ruby Doc Regex: [https://ruby-doc.org/core-2.7.1/Regexp.html](https://ruby-doc.org/core-2.7.1/Regexp.html)
3. Substrings in Ruby: [https://www.rubyguides.com/2015/03/ruby-strings/](https://www.rubyguides.com/2015/03/ruby-strings/)