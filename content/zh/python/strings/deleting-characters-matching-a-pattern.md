---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "\u5982\u4F55\u64CD\u4F5C: ."
lastmod: '2024-04-04T01:27:47.094887-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u5220\u9664\u5339\u914D\u6A21\u5F0F\u7684\u5B57\u7B26"
weight: 5
---

## 如何操作:
```Python
import re

# 示例字符串
text = "Hello, World! 1234"

# 删除所有数字
no_digits = re.sub(r'\d', '', text)
print(no_digits)  # 输出："Hello, World! "

# 删除标点符号
no_punctuation = re.sub(r'[^\w\s]', '', text)
print(no_punctuation)  # 输出："Hello World 1234"

# 删除元音
no_vowels = re.sub(r'[aeiouAEIOU]', '', text)
print(no_vowels)  # 输出："Hll, Wrld! 1234"
```

### 我写的自定义函数

鉴于我频繁进行此操作，我将其重构为了`delete()`函数。这也是[doctests](https://docs.python.org/3/library/doctest.html)的一个好示例：

```python
def delete(string: str, regex: str) -> str:
    """
    >>> delete("Hello, world!", "l")
    'Heo, word!'

    >>> delete("Hello, world!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", string)
```

## 深入探讨
在文本中删除与模式匹配的字符的做法在计算机科学中有着悠久的历史，可以追溯到早期的Unix工具，如`sed`和`grep`。在Python中，`re`模块提供了这种能力，利用正则表达式—一个用于文本处理的强大且灵活的工具。

`re`模块的替代方案包括：
- 对简单情况使用字符串方法如`replace()`。
- 对更复杂的模式和更好的Unicode支持使用第三方库如`regex`。

在底层，当你使用`re.sub()`时，Python解释器会将模式编译成一系列字节码，由一个状态机处理，直接在输入文本上执行模式匹配。对于大字符串或复杂模式，这一操作可能会非常耗资源，因此，在处理大数据时，性能考虑至关重要。

## 另见
- [Python `re`模块文档](https://docs.python.org/3/library/re.html)：Python中正则表达式的官方文档。
- [Regular-Expressions.info](https://www.regular-expressions.info/)：正则表达式的综合指南。
- [Real Python教程关于regex](https://realpython.com/regex-python/)：Python中正则表达式的实际应用。
