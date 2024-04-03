---
date: 2024-01-20 17:58:34.274117-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Python\
  \ \u092E\u0947\u0902 text search \u0914\u0930 replace \u0915\u0930\u0928\u093E \u0938\
  \u0930\u0932 \u0939\u0948\u0964 \u0926\u0947\u0916\u093F\u090F \u0915\u0948\u0938\
  \u0947."
lastmod: '2024-03-13T22:44:51.564087-06:00'
model: gpt-4-1106-preview
summary: "Python \u092E\u0947\u0902 text search \u0914\u0930 replace \u0915\u0930\u0928\
  \u093E \u0938\u0930\u0932 \u0939\u0948\u0964 \u0926\u0947\u0916\u093F\u090F \u0915\
  \u0948\u0938\u0947."
title: "\u092A\u093E\u0920 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\
  \u0932\u0928\u093E"
weight: 10
---

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
