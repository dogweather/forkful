---
title:                "Python: 字符串连接"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/concatenating-strings.md"
---

{{< edit_this_page >}}

为什么：字符串连接是编程中常用的技巧，它能够将多个字符串合并为一个字符串，为我们的代码提供更加灵活的处理方式。

如何：使用Python中的加号运算符可以简单地实现字符串连接，如下所示：

```Python
# 创建字符串
string1 = "Hello"
string2 = "World"

# 使用加号运算符实现字符串连接
concatenated = string1 + string2

# 打印输出结果
print(concatenated)
```

运行结果为：

```
HelloWorld
```

深入了解：在Python中，我们也可以使用`join()`方法来连接多个字符串，它接受一个可迭代对象作为参数，并在每个字符串之间插入指定的分隔符。示例如下：

```Python
# 创建字符串列表
strings = ["I", "love", "coding"]

# 使用join()方法连接字符串，并以空格作为分隔符
joined = " ".join(strings)

# 打印输出结果
print(joined)
```

运行结果为：

```
I love coding
```

另外，对于较长的字符串连接，我们也可以使用多行字符串（Triple-quoted strings）来实现，如下所示：

```Python
# 创建三行字符串
line1 = "Python is a great"
line2 = "language for"
line3 = "programming"

# 使用并置运算符连接字符串
concatenated = line1 + \
               line2 + \
               line3

# 打印输出结果
print(concatenated)
```

运行结果为：

```
Python is a great language for programming
```

参考链接：

- [Python字符串连接（官方文档）](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- [Python字符串连接详解](https://www.runoob.com/w3cnote/python-strings-connection-via-joining.html)

进一步阅读：

- [Python字符串操作指南](https://realpython.com/python-strings/)
- [Python字符串格式化指南](https://realpython.com/python-string-formatting/)
- [Python字符串方法总结](https://www.cnblogs.com/yangyubo/archive/2012/09/19/2696131.html)

请参考：

- [关于Python字符串的更多用法（translated into Mandarin）](https://bbs.fishc.com/thread-56941-1-1.html)
- [Python字符串连接技巧（translated into Mandarin）](https://blog.csdn.net/yusq8/article/details/79184523)