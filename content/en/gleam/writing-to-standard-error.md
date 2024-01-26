---
title:                "Writing to standard error"
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error (stderr) is like leaving a trail of digital breadcrumbs—errors and warnings that shouldn't be in the regular output mix. Programmers use stderr to report issues without messing with the standard output (stdout), keeping logs clean and making errors easier to spot.

## How to:

In Gleam, we can write to stderr using the `io` module. Let's jump right in:

```gleam
import gleam/io

pub fn main() {
  io.stderr("Oops! Something went wrong.\n") // Send a string to stderr
}
```

Sample output when things go south:
```
Oops! Something went wrong.
```

## Deep Dive

- Historically, categorizing output into stdout and stderr has helped separate regular program results from error messages.
- Alternatives include writing errors to a file or using a logging library, but stderr is direct and already at your fingertips.
- Gleam's `io` module handles stderr, directly addressing the Erlang VM's capabilities.

## See Also

- The Gleam Book for more on error handling: [https://gleam.run/book](https://gleam.run/book)
- "The Unix Programming Environment" for historical background on Unix's stdout and stderr concepts.
