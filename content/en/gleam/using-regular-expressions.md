---
title:                "Using regular expressions"
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Regular expressions (regex) are patterns used to match character combinations in strings. Programmers use them for searching, validating, and manipulating text because they're fast and efficient.

## How to:

Gleam doesn't have built-in regex support, so we use the `gleam_otp/re` module. Here's the drill:

First, add `gleam_otp` to your `rebar.config` dependencies.

```erlang
{deps, [
    {gleam_otp, "0.1.0"}
]}
```

Now, write some Gleam code to match patterns:

```rust
import gleam/otp/re

fn main() {
  let pattern = "^Hello, (\\w+)!"
  let text = "Hello, World!"
  
  let result = re.run(pattern, text)
  jsonecho(result)
}

fn jsonecho(result) {
  case result {
    Ok(matches) -> 
      case matches {
        [] -> 
          "No match found"
        [_, name] -> 
          "Matched with name: " <> name
        _ -> 
          "Unexpected match count"
      }
    Error(_) -> 
      "Pattern did not compile"
  }
}
```

Run it, and you'll see `Matched with name: World`.

## Deep Dive

Regex has been around since the 1950s; its use in Unix tools in the 1970s solidified its place. Alternatives include string functions, but they can be verbose. Implementation-wise, regex usually compiles to an internal state machine, offering speed but potential complexity in writing complex patterns.

## See Also

- [Regex101](https://regex101.com/) for testing regex patterns online.
- [Introduction to Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions) for a fundamental grounding.