---
title:                "Finding the length of a string"
date:                  2024-01-20T17:47:31.722316-07:00
model:                 gpt-4-1106-preview
simple_title:         "Finding the length of a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string means counting the number of characters it contains. Programmers do it to validate input, slice strings, or just to know how chunky that piece of text is.

## How to:

In Gleam, figuring out a string's length is a one-liner. Here’s how you do it:

```Gleam
import gleam/io
import gleam/string

fn main() {
  let my_string = "Gleam shines!"
  let length = string.len(my_string)
  io.println(length) // Output: 12
}
```

Just use the `string.len` function and pass your string. Boom! You get the length.

## Deep Dive

Once upon a time, strings were like magical incantations—tricky to handle, with each language casting its own spells. In languages like C, you'd manually traverse a character array until you hit the null terminator (`'\0'`) to find a string's length. Painful, right?

Gleam, however, keeps it simple. It runs on the BEAM VM—home to Erlang and Elixir—which treats strings as a sequence of bytes. That’s right, bytes, not characters. This is a key point because in Unicode, characters might be more than one byte. Gleam strings are UTF-8 encoded, so a single character can be between 1 to 4 bytes.

Here's the catch—`string.len` gives you the number of bytes, not the number of Unicode grapheme clusters (what we often think of as characters). So, for ASCII strings (where each character is a single byte), the length in bytes equals the number of characters. For strings containing emoji or other multibyte characters, not so much.

For a quick solution, there's no built-in alternative in Gleam right now. You'll need to pull in a library or write some code yourself if you need to count grapheme clusters.

## See Also

Dive deeper into Gleam’s string handling in the official documentation:


And for a visual representation of graphemes, bytes, and characters, check out:

- The Unicode Grapheme Cluster viewer: [https://util.unicode.org/UnicodeJsps/list-unicodeset.jsp?a=%5B%3AGrapheme_Cluster_Break%3DControl%3A%5D&abb=on&esc=on&g=&i=](https://util.unicode.org/UnicodeJsps/list-unicodeset.jsp?a=%5B%3AGrapheme_Cluster_Break%3DControl%3A%5D&abb=on&esc=on&g=&i=)
