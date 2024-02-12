---
title:                "Searching and replacing text"
aliases: - /en/python/searching-and-replacing-text.md
date:                  2024-01-20T17:58:42.437495-07:00
model:                 gpt-4-1106-preview
simple_title:         "Searching and replacing text"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Searching and replacing text is about finding strings in a block of text and changing them to something else. Programmers do it for editing code, processing data, or automating refactoring tasks.

## How to:
```Python
# Using str.replace() for simple replacement
text = "I like Python. Python is awesome!"
text = text.replace("Python", "programming")
print(text)  # Output: I like programming. programming is awesome!

# Using re.sub() for pattern-based replacement with regex
import re
text = "Contact us at support@example.com"
new_text = re.sub(r'\b[a-zA-Z0-9.-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}\b', 'support@newdomain.com', text)
print(new_text)  # Output: Contact us at support@newdomain.com
```

## Deep Dive
In the early days of programming, text editing was a manual slog. Enter regex (regular expressions), built-in the 1950s, making searching a less headache-inducing affair. For simple replaces, `str.replace()` is your go-to. It's straightforward and great for one-off replacements. When you've got patterns, like phone numbers, emails, or dates, regex with `re.sub()` is the magic wand. It finds patterns with a special syntax and swaps them out. Keep in mind, regex can be as quirky as it's powerful; it’s a tool where you get better the more puzzles you solve.

## See Also
- [Python `str.replace()` documentation](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Python `re` module documentation](https://docs.python.org/3/library/re.html)
- [Regex101](https://regex101.com/): To test regex patterns online
- [Automate the Boring Stuff with Python](https://automatetheboringstuff.com/): A book where you can learn more about practical text processing tasks.
