---
title:                "テキストの検索と置換"
date:                  2024-01-20T17:58:29.177150-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストの検索と置換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
Searching and replacing text in Python means finding strings and swapping them with something else. Programmers do this for editing code, processing data, or automating tasks.

## How to: (方法：)
Let’s see it in action.

```Python
text = "Hello, World!"
new_text = text.replace("World", "Python")
print(new_text)
```

Output:

```
Hello, Python!
```

For multiple replacements:

```Python
import re

text = "I like cats and cats like me."
pattern = re.compile(r"cats")
new_text = pattern.sub("dogs", text)
print(new_text)
```

Output:

```
I like dogs and dogs like me.
```

## Deep Dive (深い潜水)
Searching and replacing text has a long history in computing, dating back to early text editors. Python provides simple methods like `str.replace()` and powerful ones from the `re` module.

Alternatives to `str.replace()` include `re.sub()` for regular expressions, used for pattern matching. For large-scale text processing, consider `pandas` DataFrame replace methods or third-party libraries like `NLTK` for natural language processing.

Implementation details to remember: `str.replace()` is fine for straightforward substitutions but isn't suitable for complex patterns. The `re` module handles these cases, offering more options and control.

## See Also (参照する)
- Python documentation on regular expressions: https://docs.python.org/3/library/re.html
- Pandas documentation on string methods: https://pandas.pydata.org/pandas-docs/stable/user_guide/text.html
- NLTK official website for advanced text processing: https://www.nltk.org/
