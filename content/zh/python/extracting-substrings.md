---
title:                "Python: 提取子字符串"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/extracting-substrings.md"
---

{{< edit_this_page >}}

# 为什么要提取子字符串？

提取子字符串是一个非常有用的技巧，特别是在处理文本数据时。它可以让程序员轻松地从一个较大的字符串中提取出想要的部分，而不是手动搜索和处理整个字符串。

## 如何提取子字符串

要提取子字符串，在Python中使用内置的substring()函数就可以了。该函数接受两个参数，第一个是字符串，第二个是要提取的子字符串的起始索引和结束索引。让我们来看一个例子：

```Python
my_string = "Hello World"
# 从索引2到索引7提取子字符串
substring = my_string[2:7]

print(substring)
# 输出：llo W
```

在上面的例子中，我们从`my_string`字符串中提取了从索引2到索引7的子字符串，即`llo W`。

## 深入探讨提取子字符串

除了使用索引来提取子字符串之外，我们还可以使用`find()`函数来查找并提取特定的子字符串。这个函数接受一个参数，为要查找的子字符串，然后返回它在原始字符串中的索引。让我们来看一个例子：

```Python
my_string = "Hello World"
# 使用find()函数来查找并提取子字符串"World"
substring = my_string[my_string.find("World"):]

print(substring)
# 输出：World
```

在上面的例子中，我们使用了`find()`函数来查找并提取了子字符串`World`。这个函数会返回子字符串在原始字符串中的索引，并将其作为起始索引来提取子字符串。

# 参考链接

- Python官方文档：https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str