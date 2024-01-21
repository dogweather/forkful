---
title:                "Пошук та заміна тексту"
date:                  2024-01-20T17:58:33.175894-07:00
model:                 gpt-4-1106-preview
simple_title:         "Пошук та заміна тексту"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Searching and replacing text means finding specific strings in data and swapping them out for something else. Programmers do it to update data, correct errors, or process text dynamically.

## How to: (Як це зробити:)
```python
# Let's dive into code
text = "Hello, dear friend! Are you learning Python today?"

# Searching and replacing
new_text = text.replace("friend", "reader")
print(new_text)  # Output: Hello, dear reader! Are you learning Python today?

# Case-insensitive replacing using re module
import re
case_insensitive_text = re.sub("python", "Java", text, flags=re.IGNORECASE)
print(case_insensitive_text)  # Output: Hello, dear friend! Are you learning Java today?
```

## Deep Dive (Поглиблене вивчення)
The concept of searching and replacing text is as old as computing itself. Think punch cards where text was 'replaced' by physically changing the cards. Now, Python offers the `replace()` method — simple and efficient. For patterns and advanced manipulations, we turn to the `re` module. It's your go-to tool for complex scenarios: case-insensitive, regex patterns, etc. Alternatives? Sure, modules like `string` in older Python versions, or just iterating over strings with loops. Yet, `re.replace()` and `str.replace()` are go-tos for performance and convenience.

## See Also (Дивіться також)
- Python's official documentation for the `re` module: https://docs.python.org/3/library/re.html
- Detailed tutorial on regular expressions in Python: https://realpython.com/regex-python/
- For old-schoolers, the 'string' module documentation (though often obsolete): https://docs.python.org/3/library/string.html