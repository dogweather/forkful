---
title:                "删除符合模式的字符"
html_title:           "Python: 删除符合模式的字符"
simple_title:         "删除符合模式的字符"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

删除与模式匹配的字符是一项常见的编程任务，它可以帮助程序员根据特定的规则来筛选和替换文本中的字符。程序员通常需要这样做来调整数据，清洗文本，或者提高程序的可读性。

## 如何：

```Python
# 示例1：删除所有数字

import re

text = "Hello 123 World"
pattern = r"\d"

result = re.sub(pattern, "", text)
print(result)

# 输出：Hello World

# 示例2：替换所有大写字母为小写字母

text = "Hello World"
pattern = r"[A-Z]"

result = re.sub(pattern, lambda x: x.group().lower(), text)
print(result)

# 输出：hello world
```

## 深入了解：

1. 历史背景：删除字符匹配的技术由Unix系统的“ed”编辑器和“sed”流编辑器首先引入。
2. 其他方法：除了使用Python的内置模块re外，还可以使用Python的字符串方法来删除与模式匹配的字符。
3. 实现细节：除了使用正则表达式语法来定义模式外，还可以使用参数来设置替换的次数，从而更灵活地删除字符。

## 查看更多：

1. [Python官方文档-字符串方法](https://docs.python.org/3/library/stdtypes.html#str.replace)
2. [Python官方文档-re模块](https://docs.python.org/3/library/re.html)
3. [正则表达式语法参考指南](https://developers.google.com/edu/python/regular-expressions?hl=zh-cn)