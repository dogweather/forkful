---
title:                "पाठ खोजना और बदलना"
aliases:
- hi/python/searching-and-replacing-text.md
date:                  2024-01-20T17:58:34.274117-07:00
model:                 gpt-4-1106-preview
simple_title:         "पाठ खोजना और बदलना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

Text search और replace का मतलब है, एक स्ट्रिंग में खास अक्षर या वाक्यांशों को ढूंढकर उन्हें दूसरे अक्षर या वाक्यांशों से बदल देना। Programmers इसे डाटा को संपादित करने, बग ठीक करने, और कोड को refine करने के लिए करते हैं।

## How to: (कैसे करें:)

Python में text search और replace करना सरल है। देखिए कैसे:

```python
# Python में text replace करना

# एक सिंपल स्ट्रिंग
text = "Hello, World! World is beautiful."

# 'World' को 'Earth' से replace करना
new_text = text.replace("World", "Earth")
print(new_text)
```

Sample Output:

```
Hello, Earth! Earth is beautiful.
```

## Deep Dive (गहराई में जानकारी):

Text search और replace एक प्राचीन जरूरत है, जिसे पहले manually किया जाता था। Computer के आने के बाद, यह काम आसान हो गया। Python का `replace()` method इसे और भी सपाट बनाता है। लेकिन, कभी-कभी complex patterns ढूंढने के लिए हमें regular expressions (regex) की जरूरत पड़ सकती है। Python के `re` module में `sub()` function regex के साथ replace करने का ऑप्शन देता है।

```python
import re

# Regex के साथ replace करना
text = "Hello, World! World is beautiful. World is vast."

# 'World' को 'Earth' से replace करना, लेकिन केवल पहले दो occurrences के लिए
new_text = re.sub("World", "Earth", text, 2)
print(new_text)
```

Sample Output:

```
Hello, Earth! Earth is beautiful. World is vast.
```

Alternatives में text editors (जैसे Vim, Emacs), IDEs (जैसे PyCharm, VSCode), और command-line tools (जैसे sed) शामिल हैं।

## See Also (और भी देखें):

इस topic पर और जानने के लिए ये links उपयोगी होंगे:

- Python `re` module documentation: https://docs.python.org/3/library/re.html
- An interactive regex tester: https://regex101.com/
- A beginner's tutorial for understanding regular expressions in Python: https://realpython.com/regex-python/
