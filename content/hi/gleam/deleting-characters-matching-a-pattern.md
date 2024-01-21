---
title:                "पैटर्न से मेल खाते अक्षरों को हटाना"
date:                  2024-01-20T17:42:42.760196-07:00
model:                 gpt-4-1106-preview
simple_title:         "पैटर्न से मेल खाते अक्षरों को हटाना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Character matching patterns ko delete karna basically text processing ka ek part hai, jahan kisi specific pattern wale characters ko text se hata diya jata hai. Programmers data ko saaf karne, unnecessary ya sensitive information ko remove karne ke liye aisa karte hain.

## How to: (कैसे करें:)
Gleam mein characters matching a pattern ko delete karne ke liye aap pattern matching aur string functions ka use kar sakte hain. Ye raha ek simple example:

```gleam
import gleam/regex
import gleam/string.{from_slice}
import gleam/option.{Option, Some, None}

pub fn delete_pattern(text: String, pattern: String) -> Option<String> {
  let re = regex.from_string(pattern)
  match re {
    Ok(regex) -> Some(regex.replace(text, from_slice(""), 0))
    Err(_) -> None
  }
}

pub fn main() {
  let original_text = "Hello 123, hello 456"
  let cleaned_text = delete_pattern(original_text, "[0-9]+")
  assert cleaned_text == Some("Hello , hello ")
}
```
Ish example mein hum numbers ko pattern use karke text se delete kar rahe hain.

## Deep Dive (गहराई में जानकारी):
Pattern matching ki madad se characters ko delete karna ancient text processing tools, jaise ki sed aur grep jaise UNIX utilities se le kar aaya hai. Regex (regular expressions) ka istemaal karke pattern matching ki jaati hai, jo ki ek powerful tool hai lekin complex ho sakta hai.

Gleam mein `regex` library ka use karke hum easily patterns ko define kar sakte hain aur `replace` function ki help se unhe delete bhi kar sakte hain. Gleam's strong type system and error handling isko dusre languages jaise ki Python ya JavaScript ke regular expressions se thoda alag bana deti hai.

Alternatives mein aap built-in string functions ko use kar sakte hain agar aapko simpler pattern handle karne ho.

## See Also (इसे भी देखें):
- Gleam's `regex` library documentation: https://hexdocs.pm/gleam_stdlib/gleam/regex/
- Regex tutorial for beginners: https://www.regular-expressions.info/tutorial.html
- Gleam's official guide on pattern matching: https://gleam.run/book/tour/pattern-matching

इन resources se aap aur bhi gahrai se regex aur pattern matching ke baare me jaan sakte hain, aur apne Gleam projects mein effectively use kar sakte hain.