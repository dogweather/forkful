---
title:                "Python: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 为什么

在编程中，有时候字符串的大小写会影响到程序的运行。通过将字符串转换为小写，可以确保字符串的一致性，从而避免潜在的错误。

# 如何做

```Python
string = "HELLO WORLD"
lower_case = string.lower()
print(lower_case) # output: hello world
```

首先，我们需要将字符串赋值给一个变量，然后使用`lower()`方法将字符串转换为小写。最后，使用`print`函数输出结果。

# 深入探讨

字符串是由字符组成的序列，在Python中被认为是不可变的数据类型。这意味着一旦字符串被创建，就不可以直接修改。因此，转换字符串大小写时，实际上是创建了一个新的字符串对象。

在Python中，可以使用`upper`和`lower`方法分别将字符串转换为大写和小写。除此之外，还有`capitalize`和`title`方法可以将字符串的首字母或每个单词的首字母转换为大写。

值得注意的是，`lower`方法不仅仅适用于英文字符串，也可以用于其他语言的字符串。它会根据Unicode标准将字符串中的大写字符转换为小写字符。

# 参考资料

* [Python 字符串大小写转换](https://www.runoob.com/python3/python3-upper-lower.html)
* [官方文档：字符串方法](https://docs.python.org/3.8/library/stdtypes.html#string-methods)
* [Unicode 字符集](https://unicode.org/)

# 参见

* [Python 字符串操作指南](https://www.cnblogs.com/nbkhic/p/9055859.html)
* [掌握 Python 字符串方法](https://www.jianshu.com/p/7d8c6eb4285a)
* [Unicode 与 Python 字符串处理](https://www.codenong.com/28896894/)