---
date: 2024-02-03 19:02:48.155349-07:00
description: "Regular expressions (regex) in Elixir are used for searching, matching,\
  \ and manipulating strings based on specific patterns. Programmers leverage regex\u2026"
lastmod: '2024-03-13T22:44:59.773761-06:00'
model: gpt-4-0125-preview
summary: Regular expressions (regex) in Elixir are used for searching, matching, and
  manipulating strings based on specific patterns.
title: Using regular expressions
weight: 11
---

## What & Why?

Regular expressions (regex) in Elixir are used for searching, matching, and manipulating strings based on specific patterns. Programmers leverage regex for tasks like validating formats (email, URLs), parsing logs, or data extraction, thanks to its efficiency and versatility in string handling.

## How to:

Elixir uses the `Regex` module, leveraging Erlang's regex library, for regex operations. Here are basic uses:

```elixir
# Matching a pattern - Returns the first match
match_result = Regex.run(~r/hello/, "hello world")
IO.inspect(match_result) # Output: ["hello"]

# Finding all matches
all_matches = Regex.scan(~r/\d/, "There are 2 apples and 5 oranges.")
IO.inspect(all_matches) # Output: [["2"], ["5"]]

# Replacing parts of a string
replaced_string = Regex.replace(~r/\s+/, "Elixir is fun", "_")
IO.inspect(replaced_string) # Output: "Elixir_is_fun"
```

For more complex patterns and functionalities, you might consider using third-party libraries, though for most core string and pattern matching tasks, Elixir's built-in `Regex` module is quite powerful.

To perform a case-insensitive match, use the `i` option:

```elixir
case_insensitive_match = Regex.run(~r/hello/i, "Hello World")
IO.inspect(case_insensitive_match) # Output: ["Hello"]
```

Regex expressions can be precompiled for efficiency when used multiple times:

```elixir
precompiled_regex = Regex.compile!("hello")
match_result_precompiled = Regex.run(precompiled_regex, "hello world")
IO.inspect(match_result_precompiled) # Output: ["hello"]
```

Elixir also supports named captures, which can be very handy for extracting specific parts of a string while making your code more readable:

```elixir
date_string = "2023-04-15"
pattern = ~r/(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/
{:ok, captures} = Regex.run(pattern, date_string, capture: :all_names)
IO.inspect(captures) # Output: %{"year" => "2023", "month" => "04", "day" => "15"}
```

This brief overview underscores the ease with which Elixir handles regular expressions, enabling powerful string manipulation and data extraction techniques.
