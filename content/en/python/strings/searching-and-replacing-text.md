---
date: 2024-01-20 17:58:42.437495-07:00
description: "Searching and replacing text is about finding strings in a block of\
  \ text and changing them to something else. Programmers do it for editing code,\u2026"
lastmod: '2024-03-13T22:44:59.694868-06:00'
model: gpt-4-1106-preview
summary: Searching and replacing text is about finding strings in a block of text
  and changing them to something else.
title: Searching and replacing text
weight: 10
---

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
