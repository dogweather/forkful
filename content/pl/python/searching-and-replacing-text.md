---
title:                "Wyszukiwanie i zamiana tekstu"
date:                  2024-01-20T17:58:32.212117-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Szukanie i zamiana tekstu to podstawa edycji - chodzi o zmianę jednego ciągu znaków na inny. Programiści robią to, by szybko poprawiać błędy, aktualizować dane lub zmieniać kod.

## How to (Jak to zrobić):
Python makes text manipulation easy. Here are simple examples using the `replace()` method and regular expressions.

```python
# Using replace() method
text = "I love programming in Python!"
new_text = text.replace("love", "enjoy")
print(new_text)
# Output: I enjoy programming in Python!

# Using regular expressions
import re

text = "Contact us at support@example.com"
new_text = re.sub(r"support@example.com", r"helpdesk@example.pl", text)
print(new_text)
# Output: Contact us at helpdesk@example.pl
```
Remember, `replace()` is good for simple, direct substitutions, while regular expressions are more powerful for patterns.

## Deep Dive (Głębsze spojrzenie):
Searching and replacing text isn't new. It traces back to the earliest text editors in computing history, like `ed` and `sed` in Unix.

Alternatives to Python's `replace()` and `re` module include third-party libraries like `regex` for complex pattern matching. And for large-scale text processing, tools like AWK and Perl are often used for their powerful text-processing capabilities.

As for implementation, `replace()` is straightforward but can't handle patterns. Regular expressions with `re.sub()` allow for pattern matching and complex replacements but can be slower and harder to read.

## See Also (Zobacz także):
- Python `re` module documentation: https://docs.python.org/3/library/re.html
- Regular Expressions tutorial: https://www.regular-expressions.info/
- Automate the Boring Stuff with Python (text processing chapter): https://automatetheboringstuff.com/chapter8/
