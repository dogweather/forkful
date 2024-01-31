---
title:                "使用正则表达式"
date:                  2024-01-19
simple_title:         "使用正则表达式"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
正则表达式是文本处理的强大工具，用于匹配、搜索和替换文本模式。程序员使用它们来简化复杂的字符串操作，高效地处理数据。

## How to (怎么做)
```Python
import re

# 匹配邮箱
pattern = re.compile(r'\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b')
text = "联系我: example@mail.com"
match = pattern.search(text)
print(match.group())  # 输出: example@mail.com

# 替换字符串中的数字为星号
replaced_text = re.sub(r'\d', '*', '密码1234')
print(replaced_text)  # 输出: 密码****
```

## Deep Dive (深入了解)
正则表达式起源于1950年代的神经生物学。后来, 在1960年代被引入计算机科学用于字符串处理。Python内置 `re` 模块就是它的实现之一。虽然正则表达式非常强大，它也有一些缺点，比如可读性差和某些情况下的效率不高。其他文本处理方法，比如字符串方法或解析库（如`pyparsing`），可以作为替代选择在特定场景下使用。

## See Also (另请参阅)
- Python 官方文档 `re` 模块：https://docs.python.org/3/library/re.html
- 正则表达式测试器：https://regex101.com/
- 正则表达式速查表：https://www.debuggex.com/cheatsheet/regex/python
