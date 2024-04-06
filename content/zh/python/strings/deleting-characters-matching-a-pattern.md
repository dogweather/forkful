---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "\u5982\u4F55\uFF1A \u6211\u8FD9\u6837\u505A\u7684\u9891\u7387\u8DB3\u591F\
  \u9AD8\uFF0C\u4EE5\u81F3\u4E8E\u6211\u5C06\u5176\u91CD\u6784\u6210\u4E86\u8FD9\u4E2A\
  \u7B80\u5355\u7684 `delete()` \u51FD\u6570\u3002\u8FD9\u4E5F\u662F\u5BF9[doctests](https://docs.python.org/3/library/doctest.html)\u7684\
  \u4E00\u4E2A\u5F88\u597D\u7684\u6F14\u793A\uFF1A."
lastmod: '2024-04-05T21:53:47.593995-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u5220\u9664\u5339\u914D\u6A21\u5F0F\u7684\u5B57\u7B26"
weight: 5
---

## 如何：
```Python
import re

# 示例字符串
text = "Hello, World! 1234"

# 移除所有数字
no_digits = re.sub(r'\d', '', text)
print(no_digits)  # 输出："Hello, World! "

# 移除标点
no_punctuation = re.sub(r'[^\w\s]', '', text)
print(no_punctuation)  # 输出："Hello World 1234"

# 移除元音
no_vowels = re.sub(r'[aeiouAEIOU]', '', text)
print(no_vowels)  # 输出："Hll, Wrld! 1234"
```

### 我的自定义函数

我这样做的频率足够高，以至于我将其重构成了这个简单的 `delete()` 函数。这也是对[doctests](https://docs.python.org/3/library/doctest.html)的一个很好的演示：

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
在文本中删除匹配模式的字符的做法在计算机科学中有着悠久的历史，可以追溯到早期的Unix工具，如 `sed` 和 `grep`。在Python中，`re` 模块提供了这种能力，利用正则表达式——一种强大而灵活的文本处理工具。

`re` 模块的替代品包括：
- 对于简单情况，使用字符串方法如 `replace()`。
- 对于更复杂的模式和更好的Unicode支持，使用第三方库如 `regex`。

在底层，当你使用 `re.sub()` 时，Python解释器会将模式编译成一系列的字节码，由一个状态机处理，直接在输入文本上执行模式匹配。对于大字符串或复杂模式，这个操作可能会很消耗资源，因此在大数据处理中，性能考虑至关重要。

## 另见
- [Python `re` 模块文档](https://docs.python.org/3/library/re.html)：Python中正则表达式的官方文档。
- [Regular-Expressions.info](https://www.regular-expressions.info/)：正则表达式的全面指南。
- [Real Python教程关于regex的部分](https://realpython.com/regex-python/)：Python中正则表达式的实际应用。
