---
title:                "将字符串转换为小写"
html_title:           "Python: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

许多程序员在处理文本数据时，需要将字符串转换为小写。这样可以帮助比较和搜索文本更方便，减少大小写带来的错误。

## 如何操作

首先，我们需要使用Python内置的`lower()`函数来将字符串转换为小写。下面是一个简单的例子：

```Python
# 定义一个字符串
text = "Hello WORLD"
# 使用lower()函数转换为小写
lower_text = text.lower()
# 输出转换后的字符串
print(lower_text)
```

输出结果为：

```
hello world
```

除了使用`lower()`函数外，还可以使用`.lower()`方法来实现相同的功能。例如：

```Python
# 定义一个字符串
text = "Hello WORLD"
# 使用lower()方法转换为小写
lower_text = text.lower()
# 输出转换后的字符串
print(lower_text)
```

输出结果为：

```
hello world
```

除了单独转换字符串外，也可以在比较字符串时直接将其转换为小写。例如：

```Python
# 定义两个字符串
text1 = "HELLO"
text2 = "hello"
# 将两个字符串转换为小写并比较
if text1.lower() == text2.lower():
    print("相同")
else:
    print("不相同")
```

输出结果为：

```
相同
```

## 深入探讨

在Python中，字符串是不可变对象，这意味着一旦定义，就无法直接改变其内容。因此，在使用`lower()`函数或`.lower()`方法时，实际上是创建了一个新的字符串对象，而原始字符串对象并未改变。此外，`lower()`函数和`.lower()`方法不仅仅适用于英文字符，也可以用于其他语言的字符。

## 参考链接

- Python字符串操作教程：https://www.runoob.com/python3/python3-string.html
- Python官方文档：https://docs.python.org/3/library/string.html#string.lower
- 完整的Python字符串方法列表：https://www.programiz.com/python-programming/string#methods