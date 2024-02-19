---
aliases:
- /zh/python/concatenating-strings/
date: 2024-01-20 17:35:32.868972-07:00
description: "\u5B57\u7B26\u4E32\u8FDE\u63A5\u5C31\u662F\u5C06\u591A\u4E2A\u5B57\u7B26\
  \u4E32\u62FC\u6210\u4E00\u4E2A\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\
  \u4E86\u521B\u5EFA\u52A8\u6001\u6587\u672C\u6570\u636E\u6216\u5408\u5E76\u4FE1\u606F\
  \u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:58.783815
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u8FDE\u63A5\u5C31\u662F\u5C06\u591A\u4E2A\u5B57\u7B26\
  \u4E32\u62FC\u6210\u4E00\u4E2A\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\
  \u4E86\u521B\u5EFA\u52A8\u6001\u6587\u672C\u6570\u636E\u6216\u5408\u5E76\u4FE1\u606F\
  \u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
---

{{< edit_this_page >}}

## What & Why? 什么以及为什么?
字符串连接就是将多个字符串拼成一个。程序员这样做是为了创建动态文本数据或合并信息。

## How to: 如何操作
使用加号 `+` 连接字符串：

```python
greeting = "你好"
name = "世界"
message = greeting + ", " + name + "!"
print(message)
```

输出：

```
你好, 世界!
```

使用 `.join()` 方法：

```python
words = ["Python", "很", "有趣"]
sentence = ' '.join(words)
print(sentence)
```

输出：

```
Python 很 有趣
```

使用 `f-string`：

```python
name = "小明"
age = 20
introduction = f"我叫{name}, 今年{age}岁。"
print(introduction)
```

输出：

```
我叫小明, 今年20岁。
```

## Deep Dive: 深入探讨
过去，比如Python 2时代，字符串格式化通常用 `%` 运算符。如今，`f-string` 更常用因为它简短且易读。

不同连接方式有不同的效率。`+` 对少量的字符串连接足够好，但在连接大量字符串时，`.join()` 更高效因为它不会重复创建字符串。

在内部，Python的字符串是不可变的，这意味着每次使用 `+`，实际上是创建了一个新字符串。这在拼接大量小字符串时可能导致性能问题。

## See Also: 参见其他资源
- Python 官方文档关于字符串方法: https://docs.python.org/3/library/stdtypes.html#string-methods
- Python 官方文档关于格式化字符串字面量（f-strings）: https://docs.python.org/3/reference/lexical_analysis.html#formatted-string-literals
- 维基百科关于字符串连接: https://en.wikipedia.org/wiki/Concatenation
