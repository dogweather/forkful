---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string means transforming a textually represented date into a manipulable data type. Programmers do this to utilize date/time functions, compare dates, or store dates in databases.

## How to:
Ruby makes parsing dates from strings easy peasy. Here's a simple example of this using built-in `Date.parse` function.

```Ruby
require 'date'
date_string = "2023-10-05"
date = Date.parse(date_string)
puts date
```
Output:

```Ruby
2023-10-05
```

Well, it's as simple as that. You provide a date string to `Date.parse` and it hands you a date object!

## Deep Dive
Digging a bit deeper, parsing is crucial because computers understand dates not as a string of characters but numerical representations. Notably, Euler’s algorithm powers Ruby's `Date.parse` that here leverages Europe's day-first date format.

The function tries to guess the correct format, which might occasionally invoke its downside: the risk of misunderstanding when the date parts are ambiguous (e.g., "01-02-03"). 

There are alternatives to the built-in method. For more control, use `strptime` method:

```Ruby
date = Date.strptime(date_string, "%Y-%m-%d")
```

But beware, `strptime` requires you to specify the format, thus lacks the `Date.parse` flexibility. Choose your poison depending on the trade-off you're willing to make between control and convenience.

## See Also
Delve deeper into the matter with informative resources:
- [Ruby Date.parse documentation](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html#method-c-parse)
- [Ruby Date.strptime documentation](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html#method-c-strptime)
- [Why, when & how to use parse and strptime](https://www.rubyguides.com/2015/12/ruby-time/)