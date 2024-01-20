---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string means converting written or typed text into a datetime object a program can understand. We do this because sometimes the only way to get a date out of certain systems is via text.

## How to:

Fire up your Fish Shell and let's get parsing!

```fish
# Define the date string
set dateString "2022-03-25 14:20:00"

# Parse the date string into a Unix timestamp
set parsedDate (date -u -d $dateString +%s)

# Echo the parsed date
echo $parsedDate
```
When you run this, you will see the converted Unix timestamp spit out. Pretty neat, huh?

## Deep Dive
Historically, date parsing was a necessity because legacy systems often relied on human-readable date strings. With Fish Shell, parsing dates is easy, but there are alternatives, like Python’s `dateutil.parser.parse()` or JavaScript’s `Date.parse()`.

Different systems or programming languages may store and manage dates differently, but most will convert to a Unix timestamp when parsing. This is a numerical representation of a date as the number of seconds that have passed since the "Unix Epoch", 00:00:00 Coordinated Universal Time (UTC), Thursday, 1 January 1970, not counting leap seconds.

In Fish Shell, the `-d` option is fed the date string and the `+%s` option is used to specify the output format which, in this case, is a Unix timestamp. This makes it possible to work with the parsed date.

## See Also

For more details on the Fish Shell, check out these resources:

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Python dateutil Library](https://dateutil.readthedocs.io/en/stable/)
- [JavaScript Date.parse()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse)