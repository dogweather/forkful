---
title:                "Searching and replacing text"
date:                  2024-01-20T17:57:56.737141-07:00
model:                 gpt-4-1106-preview
simple_title:         "Searching and replacing text"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text involves finding specific strings within a larger text body and swapping them for alternative strings. Programmers do this for tasks like updating code, correcting errors, or processing data formats.

## How to:

In Gleam, let's keep it straightforward. You want to search for "world" and replace it with "Gleam". Here's the code:

```gleam
import gleam/string

pub fn replace_world_with_gleam(input: String) -> String {
  string.replace(input, "world", "Gleam")
}

pub fn main() {
  let text = "Hello, world!"
  let new_text = replace_world_with_gleam(text)
  new_text
}
```

Sample output:

```
"Hello, Gleam!"
```

No muss, no fuss – works like a charm.

## Deep Dive

Historically, searching and replacing text is as old as the hills in programming terms. It’s basic but essential, like screwdrivers to a carpenter. In the functional family, we’re all about strings without side effects. In Gleam, the `string` module is your go-to, it’s built-in and ready to rock. No reinventing the wheel here.

Alternatives? Sure, you could go regex if your needs get complex, or maybe dive into different libraries or languages, but for many tasks, Gleam's `string.replace` hits the mark. It's neat, functional (Gleam’s a strong, statically typed language in the ML family), and it plugs into the BEAM ecosystem – same as Erlang and Elixir.

When you run `string.replace`, you trigger a sequence of character pattern matching underneath. It's efficient and does the job without you needing to worry about the nitty-gritty. Gleam's guiding principle is to keep things type-safe and concise – it holds true for string manipulation too.

## See Also

- Gleam's String module documentation: https://hexdocs.pm/gleam_stdlib/gleam/string/
- Regular expressions in Gleam with the `gleam_regex` library: https://hex.pm/packages/gleam_regex
- For a bigger picture, see Gleam’s general philosophy: https://gleam.run/book/tour/philosophy.html

Whether your task is simple or complex, Gleam's got your back for all things strings. Happy coding!