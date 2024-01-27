---
title:                "Використання регулярних виразів"
date:                  2024-01-19
html_title:           "Bash: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
(## Що та Чому?)
Regular expressions (regexes) are patterns used to match character combinations in text strings. Programmers use regexes for searching, editing, or validating text because they're fast and versatile.

## How to:
(## Як це зробити:)
```Python
import re

# Example 1: Finding email addresses
text = "Send your feedback to hello@example.com or contact@example.org"
emails = re.findall(r'[\w\.-]+@[\w\.-]+', text)
print(emails)

# Example 2: Replacing all digits in a string with a dash
text_with_numbers = "My phone number is 123-456-7890"
replaced_text = re.sub(r'\d', '-', text_with_numbers)
print(replaced_text)

# Example 3: Validating a password strength
password = "StrongPassw0rd!"
is_strong = bool(re.fullmatch(r'^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[@$!%*?&])[A-Za-z\d@$!%*?&]{8,}$', password))
print(is_strong)
```
_Output:_
```
['hello@example.com', 'contact@example.org']
My phone number is ---_---_----
True
```

## Deep Dive:
(## Поглиблений Розгляд:)
Regular expressions originated in the 1950s with formal language theory. Alternatives to regex include string methods like `find()` or `replace()`, or using parsers for complex text structure. Python implements regex via the `re` module, which uses a backtracking algorithm, meaning it can get slow for complex patterns.

## See Also:
(## Додатково:)
- Python's `re` module documentation: https://docs.python.org/3/library/re.html
- Regex101 (for testing regex patterns): https://regex101.com/
- Automate the Boring Stuff with Python (Chapter on regex): https://automatetheboringstuff.com/chapter7/
