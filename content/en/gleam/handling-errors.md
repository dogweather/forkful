---
title:                "Handling errors"
date:                  2024-01-21T21:19:22.031988-07:00
model:                 gpt-4-1106-preview
simple_title:         "Handling errors"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/handling-errors.md"
---

{{< edit_this_page >}}

## What & Why?
Handling errors is about anticipating things going awry in your code and managing those situations gracefully. Programmers do this because it keeps applications robust and user-friendly, even when faced with the unexpected.

## How to:
In Gleam, you'll often make use of the `Result` type for error handling. It's an enum with two variants: `Ok` (for success) and `Error` (for failure). Here's a simple example:

```Gleam
pub fn might_fail(break_it: Bool) -> Result(Int, String) {
  if break_it {
    Error("Oops! It broke.".to_string())
  } else {
    Ok(42)
  }
}

pub fn main() {
  let result = might_fail(False)
  case result {
    Ok(value) => value
    Error(message) => {
      io.println(message)
      0
    } 
  }
}
```

If you run `main` with `might_fail(False)`, it'll return `42`. If you pass `True`, it prints "Oops! It broke." and returns `0`.

## Deep Dive
Gleam's approach to error handling is influenced by its Erlang roots. Historically, Erlang uses a "let it crash" philosophy, relying on supervision trees to manage process failures. However, when you're writing Gleam code that's not inside a process meant to be supervised, like within a library function, you'd want to handle errors explicitly.

Alternatives to using `Result` include using the `Option` type for cases where something might be `None` (nothing) or `Some` (something), but these don't carry error information. For signaling errors across process boundaries, you might use Erlang's message-passing mechanisms.

Gleam's error handling reflects a functional programming style, where side-effects (like errors) are managed with types and pattern-matching, providing clarity and predictability in error management.

## See Also
- [Erlang's Error Handling](http://erlang.org/doc/reference_manual/errors.html)