---
date: 2024-01-20 17:35:32.868972-07:00
description: "How to: \u5982\u4F55\u64CD\u4F5C \u8FC7\u53BB\uFF0C\u6BD4\u5982Python\
  \ 2\u65F6\u4EE3\uFF0C\u5B57\u7B26\u4E32\u683C\u5F0F\u5316\u901A\u5E38\u7528 `%`\
  \ \u8FD0\u7B97\u7B26\u3002\u5982\u4ECA\uFF0C`f-string` \u66F4\u5E38\u7528\u56E0\u4E3A\
  \u5B83\u7B80\u77ED\u4E14\u6613\u8BFB\u3002 \u4E0D\u540C\u8FDE\u63A5\u65B9\u5F0F\u6709\
  \u4E0D\u540C\u7684\u6548\u7387\u3002`+` \u5BF9\u5C11\u91CF\u7684\u5B57\u7B26\u4E32\
  \u8FDE\u63A5\u8DB3\u591F\u597D\uFF0C\u4F46\u5728\u8FDE\u63A5\u5927\u91CF\u5B57\u7B26\
  \u4E32\u65F6\uFF0C`.join()` \u66F4\u9AD8\u6548\u56E0\u4E3A\u5B83\u4E0D\u4F1A\u91CD\
  \u590D\u521B\u5EFA\u5B57\u7B26\u4E32\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.472630-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u64CD\u4F5C \u8FC7\u53BB\uFF0C\u6BD4\u5982Python 2\u65F6\u4EE3\
  \uFF0C\u5B57\u7B26\u4E32\u683C\u5F0F\u5316\u901A\u5E38\u7528 `%` \u8FD0\u7B97\u7B26\
  \u3002\u5982\u4ECA\uFF0C`f-string` \u66F4\u5E38\u7528\u56E0\u4E3A\u5B83\u7B80\u77ED\
  \u4E14\u6613\u8BFB\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
weight: 3
---

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
