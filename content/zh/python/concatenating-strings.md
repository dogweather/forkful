---
title:                "字符串连接"
html_title:           "Python: 字符串连接"
simple_title:         "字符串连接"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/concatenating-strings.md"
---

{{< edit_this_page >}}

什么是字符串合并？为什么程序员要这么做？

字符串合并是指将两个或多个字符串连接起来，创建一个新的字符串。程序员常常需要合并字符串来创建动态的或者格式化的输出。比如，当在网站上显示用户的名字和姓氏时，程序员需要将这两个字符串合并起来。

如何实现字符串合并：

```Python
name = "张"
surname = "三"
full_name = name + surname
print(full_name)
```

输出： 张三

```Python
greeting = "你好"
name = "李四"
message = greeting + name + "，欢迎来到我的网站！"
print(message)
```

输出： 你好李四，欢迎来到我的网站！

深入探讨：

1. 历史背景：字符串合并最早是在 1950 年代提出的概念。在早期计算机系统中，字符串是一连串的字节，程序员必须手动处理它们来实现合并操作。

2. 替代方法：除了使用加号来合并字符串，程序员也可以使用字符串格式化来实现，比如使用 f-strings 或者 .format() 方法。

3. 实现细节：在 Python 中，字符串是不可变的，所以每次合并都会创建一个全新的字符串。

相关资料：

- Python 字符串教程：https://docs.python.org/zh-cn/3/tutorial/introduction.html#strings

- 字符串格式化指南：https://realpython.com/python-string-formatting/#2-string-formatting-with-the-format-method

- f-strings 文档：https://realpython.com/python-f-strings/#the-string-representation-of-object-types

希望本文能帮助你理解并学习字符串合并的基础知识。继续探索后面的章节，你将学会更多关于字符串的用法。祝学习愉快！