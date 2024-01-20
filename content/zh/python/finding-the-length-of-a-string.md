---
title:                "查找字符串的长度"
html_title:           "Javascript: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？

在 python 中获取字符串长度实际上就是知道字符串包含多少字符。为什么呢？因为它让程序员能对字符串进行更精细的控制，从而满足特定的编程需求。

## 如何做：

在 python 中，我们使用 len() 函数来获取字符串的长度。看下面的代码和输出：

```Python
my_string = "Hello, World!"
print(len(my_string))
```

输出：

```Python
13
```

在以上示例中，"Hello, World!" 包含13个字符，包括空格和标点符号，所以输出结果是13。

## 深入挖掘：

1. 历史背景: len() 函数是 Python 的内置函数，存在于 Python 早期版本，并一直沿用至今。

2. 替代方法: 如果你出于某种原因不使用 len() 函数，你还可以循环遍历字符串来计数字符。但这通常效率低下，不推荐使用。

```Python
my_string = "Hello, World!"
counter = 0
for character in my_string:
    counter += 1
print(counter)
```

3. 实现细节: Python 的 len() 函数使用对象内部的 `__len__()` 方法来获取长度。对于字符串对象，`__len__()` 方法将返回字符的数量。

## 另请参阅：

1. [Python 官方文档 - len() 函数](https://docs.python.org/zh-cn/3/library/functions.html#len)
2. [String Methods in Python - Tutorialspoint](https://www.tutorialspoint.com/python/python_strings.htm)
3. [Understand Python String Length with Examples](https://pythonguides.com/python-string-length/)