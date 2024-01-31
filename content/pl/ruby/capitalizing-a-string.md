---
title:                "Zamiana liter na wielkie w ciągu znaków"
date:                  2024-01-19
html_title:           "Arduino: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Capitalizing a string means converting the first character to uppercase if it's a letter. Programmers do this for consistency in user interfaces, for proper nouns, or to match stylistic or language rules.

## How to: (Jak to zrobić:)
```Ruby
# Capitalizing the first letter of a string
phrase = "warszawa jest super!"
puts phrase.capitalize   # Output: "Warszawa jest super!"

# Capitalizing all words in a string
require 'active_support/core_ext/string/inflections'
puts phrase.titleize     # Output: "Warszawa Jest Super!"

# Using 'capitalize' with bang (!) method for in-place modification
phrase.capitalize!
puts phrase              # Output: "Warszawa jest super!"
```

## Deep Dive (Dogłębna analiza)
Capitalizing strings is straightforward in Ruby, but there's more to it. Historically, it's been a feature of string manipulation in many programming languages to help with formatting and presenting text data. 

There are alternatives like `upcase` for full uppercasing, `downcase` for lowercasing, and `titleize` (from the Rails framework) for capitalizing every word. These alternatives cater to different needs. 

The Ruby method `capitalize` changes the first character to uppercase and the rest to lowercase. In other programming contexts, `capitalize` might only change the first character to uppercase, keeping the rest unchanged—a good thing to keep in mind when switching languages.

Implementation-wise, Unicode support in capitalization can be tricky, as not all scripts have the concept of letter casing.

## See Also (Zobacz także)
- Ruby Documentation for capitalize: [ruby-doc.org/core/String.html#method-i-capitalize](https://ruby-doc.org/core/String.html#method-i-capitalize)
- Rails String Inflections for 'titleize': [api.rubyonrails.org](https://api.rubyonrails.org/classes/String.html#method-i-titleize)
