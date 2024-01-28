---
title:                "Removing quotes from a string"
date:                  2024-01-25T20:50:17.960940-07:00
model:                 gpt-4-1106-preview
simple_title:         "Removing quotes from a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Removing quotes from a string means peeling off those extra layers – the quotation marks – from your text data. Programmers do this to sanitize input, prepare strings for processing, or just to keep things tidy and consistent in their applications. It's all about clean, useable data in the end.

## How to:
Stripping quotes in Gleam is straightforward. We can use pattern matching or built-in string functions. Here's a quick example to illustrate:

```gleam
pub fn remove_quotes(text: String) -> String {
  let without_quotes = string.trim(text, "\"")
  without_quotes
}

pub fn main() {
  let text_with_quotes = "\"Hello, World!\""
  let cleaned_text = remove_quotes(text_with_quotes)
  io.println(cleaned_text)
}
```

Sample output:
```
Hello, World!
```

## Deep Dive
Historically, dealing with quotes in strings has been a common task in text processing and scripting languages. Due to the nature of strings often being user input or read from files, they can come with quotation marks that need removal for various reasons, such as database insertion or formatting.

In Gleam, we use the `string.trim` function to shave off the quotes. There are alternatives! We could loop through the string or apply regular expressions, but `string.trim` is your handy tool for the job because of its brevity and performance.

If we dive into implementation details, `string.trim` works by removing characters from the start and end of the string that match the provided pattern. So if you've got quotes at both ends of your string, they're chopped off in one go. Keep in mind that it only removes the quotes if they're at the edges; quotes sitting snug in the middle of your text will stay put.

## See Also
For the curious minds out there who want to explore more:
- [Gleam's String module documentation](https://gleam.run/stdlib/string/)
- Discussions on text processing in programming on [Stack Overflow](https://stackoverflow.com/questions/tagged/text-processing)
