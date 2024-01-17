---
title:                "求取字符串的长度"
html_title:           "Python: 求取字符串的长度"
simple_title:         "求取字符串的长度"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 什么是字符串长度？为什么程序员需要计算它？

计算一个字符串的长度意味着确定该字符串中包含多少字符。程序员通常需要这种信息来评估字符串的大小、重要性或者它是否符合所需的条件。

## 如何计算字符串长度？

您可以像这样使用内置的len()函数来计算字符串长度：

```python
string = "Hello, World!"
print(len(string)) # output: 13
```

字符串的长度也可以通过索引操作符[]来获取，如下所示：

```python
print(string[2]) # output: l
```

## 深入了解

在过去，计算字符串长度是一个比较复杂的过程，因为程序员必须手动遍历字符串中的每个字符来计算长度。但是，如今的计算机编程语言都包含了内置函数来迅速计算字符串长度。除了Python中的len()函数，另一个常用的函数是sys.getsizeof()，它可以计算任何对象的大小，包括字符串。

另一种获取字符串长度的方法是使用循环来逐个遍历字符串中的字符并计算数量。这种方法可能会比内置函数len()慢，但它可以解决某些特定的编程问题。

## 参考资料

- [Python内置函数文档](https://docs.python.org/3/library/functions.html#len)
- [Python sys.getsizeof()函数文档](https://docs.python.org/3/library/sys.html#sys.getsizeof)