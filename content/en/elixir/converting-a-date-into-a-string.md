---
title:                "Converting a date into a string"
html_title:           "Elixir recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
We convert dates to strings in programming to make them human-readable and portable. Often, we store, display, or transport dates in a string format, such as `"2022-12-01"`.

## How to:
We can use Elixir's built-in function `Date.to_string/1`. This function takes a Date struct and returns a string in the "YYYY-MM-DD" format. Let's dive into the simple example for better clarity:

```Elixir
date = Date.new(2022, 12, 1)
IO.puts Date.to_string(date)
```

The code will output:

```
"2022-12-01"
```
Easy peasy, right?

## Deep Dive
This operation of converting date to string in Elixir has its ground rooted in the ISO 8601 standard that was introduced in 1988. This standard is used because it avoids confusion between different date formats, you no need to wonder if "12-01-2022" means December 1st or January 12th.

As for alternatives, we have `NaiveDateTime.to_string/1` and `DateTime.to_string/1`, which not only enclose date but also time information. These functions are handy when you need more than just dates.

Peeking under the hood of the `Date.to_string/1`, it's interesting to know that it leans heavily on pattern matching, a technique that Elixir language borrows from the functional paradigm, making things more explicit and easy to maintain.

## See Also
For additional context and advanced usage, here are some resources you may find helpful:
- Full Elixir Language Documentation: https://hexdocs.pm/elixir/Kernel.html
- Dates and Times in Elixir: https://hexdocs.pm/elixir/Date.html
- More about ISO 8601: https://en.wikipedia.org/wiki/ISO_8601