---
title:                "连接字符串"
html_title:           "C: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## 什么与为什么？

字符串拼接是将两个或更多的字符串合并为一个单独的字符串的过程。程序员之所以这样做，是因为他们需要频繁地创建或修改字符串表达式，用于日常编程任务，如处理用户输入或生成可打印的输出。

## 如何操作：

在Python中，我们可以使用以下几种方式进行字符串拼接：

1. 使用 `+` 运算符：

```python
a = "你好"
b = "世界"
c = a + ", " + b
print(c)
```

输出：

```python
你好, 世界
```

2. 使用 `join()` 函数：

```python
words = ["python", "简单", "有用"]
sentence = " ".join(words)
print(sentence)
```

输出：

```python
python 简单 有用
```


## 深入探讨

历史上，字符串拼接是任何一种编程语言中都有的基本特性，包括Python。然而，随着Python的发展，现在有更多的方式来实现字符串的拼接，例如上面提到的 `join()` 方法。

在某些情况下， `join()` 可能会比直接使用 `+` 运算符进行拼接更有效率。这是因为 `join()` 在内部使用了更复杂的算法来优化字符串拼接的性能。

除了 `+` 和 `join()`，Python 还提供了一些其他的方法，如格式化字符串或使用字符串模板对象来进行字符串拼接。

## 另请参阅

以下是一些有关Python字符串拼接更多信息和教程的链接：

- [Python字符串拼接—— Python 官方文档](https://docs.python.org/zh-cn/3/tutorial/introduction.html#strings)
- [String Concatenation in Python - Real Python](https://realpython.com/python-string-concatenation/)