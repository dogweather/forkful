---
title:                "正規表現の使用"
html_title:           "C: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
## 何となぜ？

Regular expressions (regex) allow you to search for patterns in text. Programmers use them for tasks like validity checking, searching, and text processing because of their power to work with strings effectively.

## How to:
## 方法：

Let's search for dates in the format 'YYYY-MM-DD'.

```python
import re

text = 'Important dates are 2023-04-01 and 2024-08-21.'
pattern = r'\d{4}-\d{2}-\d{2}'

matches = re.findall(pattern, text)
print(matches)
```

Output:

```
['2023-04-01', '2024-08-21']
```

Now, let's extract the year, month, and day separately.

```python
match = re.search(pattern, text)
year, month, day = match.groups()
print(f"Year: {year}, Month: {month}, Day: {day}")
```

Error – No group in pattern. Let's revise the pattern to include groups.

```python
pattern = r'(\d{4})-(\d{2})-(\d{2})'
match = re.search(pattern, text)

if match:
    year, month, day = match.groups()
    print(f"Year: {year}, Month: {month}, Day: {day}")
```

Output:

```
Year: 2023, Month: 04, Day: 01
```

## Deep Dive:
## 徹底解説：

Regular expressions originated in the 1950s with mathematician Stephen Kleene. They've become integral in text processing in Unix, and by extension, in modern programming languages. Alternatives include string methods like `str.find()` or `str.split()`, but these lack pattern matching capabilities. Python uses a library called `re`, which implements regex according to the Perl standard, known for its flexibility and speed.

## See Also:
## 参照：

- Python's re module documentation: https://docs.python.org/3/library/re.html
- Regular expression basics: https://www.regular-expressions.info/
- Regex testing tool: https://regex101.com/