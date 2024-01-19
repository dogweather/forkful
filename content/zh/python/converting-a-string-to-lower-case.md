---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么与为什么

将字符串转换为小写是一种常见的Python编程操作，它将所有字符串中的大写字符更改为相应的小写字符。这一操作可能出于字符串统一、比较或搜索等需要。 

## 如何做

下面是将字符串转换为小写的Python代码示例：
```Python
# 定义一个字符串
my_string = "Hello World"

# 使用lower方法转换为小写
lowercase_string = my_string.lower()

# 输出结果
print(lowercase_string)
```
您可以运行上述代码，输出的结果将是：
```
hello world
```

## 深入了解

1. 历史背景： Python是一种强大且易用的编程语言，自从1989年以来，已经在文本处理、数据科学、网络开发等多个领域广受欢迎。其中，lower()函数是Python string模块中自始至终都有的一部分。
2. 替代方案：虽然大部分情况下，lower()函数已经能满足要求，当然还有其他方式可以达到类似效果，例如使用translate()和maketrans()函数与特殊的Unicode大小写转换表。
3. 实现细节： 在Python内部，lower()函数使用C的ctype库的tolowe()函数实现，它能够正确的将包括特殊字符（如UTF-8编码的非英文字符）在内的所有字母字符转换为小写。

## 参考链接

与字符串处理有关的其它Python功能：
1. Python官方文档：Python string模块 [https://docs.python.org/3/library/string.html](https://docs.python.org/3/library/string.html)
2. Python字符串的大小写转换教程 [https://www.w3schools.com/python/python_ref_string.asp](https://www.w3schools.com/python/python_ref_string.asp)
3. Python更深入的文本处理教程 [https://realpython.com/python-strings/](https://realpython.com/python-strings/)