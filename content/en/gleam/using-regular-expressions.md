---
title:                "Using regular expressions"
html_title:           "Gleam recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

In the Gleam universe, Regular Expressions (regex) allow us to find, match, and manipulate strings by establishing patterns. They're vital because, well - they're the ultimate power tools for string operations.

## How to:

Deploying regex in Gleam is straightforward.

Suppose we're dealing with a standard use case: searching for a pattern within a string. Let's say you're trying to find e-mail addresses in text. Here's a snippet to show you how.

```Gleam

import gleam/regex

let email_regex = regex.from_string("[a-z0-9.+_\-]+@[a-z0-9]+\.[a-z]+").unwrap()
let text = "Here's an email: person@domain.com."

regex.find(&email_regex, text)
  |> result.unwrap_or(Nil)
  |> io.println

```

In the console, you see an output exactly like this:

```Gleam
#{match=[49..62], named_groups=[], pre_match=[0..22], post_match=[62..67]}
```

## Deep Dive

The concept of regex has been around since the '50s - amazing right? Ken Thompson brought regex to prominence in the '70s with the UNIX 'ed' editor. And, thanks to the POSIX Standard, regex has become a common utility in programming languages since the '80s. 

In Gleam context, Gleam uses Rust's regex library, which is a significant advantage as it makes regex very fast. The drawback? Well, it doesn't support all regex features you might find elsewhere. 

As for alternatives, case matching and string functions can sometimes replace regex, but generally, regex is the most convenient and powerful.

## See Also:

Looking for more? Check out these links:

* [Rust regex library](https://github.com/rust-lang/regex): This is the Rust library Gleam's regex is built on. A great place for the curious. 
* [Regular-Expressions.Info](https://www.regular-expressions.info/): If regex has caught your fancy, this is a resource dedicated to all things regex. You'll get an understanding of it beyond Gleam or even Rust.