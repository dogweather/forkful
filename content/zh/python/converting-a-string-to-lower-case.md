---
title:                "Python: 将字符串转换为小写"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

#为什么

在Python编程中，经常需要对字符串进行大小写转换。将字符串转换为小写可能是因为需要进行字符串的匹配或比较，或者需要统一字符串的格式。无论是哪种情况，使用字符串的lower()方法是一个非常方便的解决方法。

#如何

下面是使用Python代码将字符串转换为小写的示例：

```Python
#定义一个字符串
s = "Hello World!"
#使用lower()方法将字符串转换为小写并赋值给新变量
lower_s = s.lower()
#打印转换后的字符串
print(lower_s)
```

输出结果为：

```
hello world!
```

在上面的示例中，首先定义了一个字符串变量s，然后使用lower()方法将其转换为小写并赋值给新变量lower_s，最后打印出转换后的字符串。可以看到，字符串中的所有字母都被转换为了小写形式。

除了使用lower()方法外，还可以使用str.casefold()方法来进行字符串转换。这两个方法都能够实现将字符串转换为小写的功能，区别在于str.casefold()方法能够处理一些特殊字符的转换。

#深入探讨

在Python中，字符串是不可变类型的数据，这意味着一旦字符串被创建，就无法改变它的值。因此，使用字符串的lower()方法并不会改变原来字符串的值，而是返回一个新的字符串。

另外，需要注意的是，lower()方法只能转换英文字符为小写，对于其他语言的字符可能会有不同的结果。此外，如果想要将字符串中的所有字符都转换为大写，可以使用upper()方法。

#参考链接

- [Python字符串转换为小写](https://www.runoob.com/python/string-lower.html)
- [Python字符串转换为小写和大写](https://blog.csdn.net/qq_24237183/article/details/76480620)
- [Python字符串大小写转换的几种方式](https://www.cnblogs.com/lxlx1798/p/10620987.html)

#另请参阅

- [Python字符串处理教程](https://www.liaoxuefeng.com/wiki/1016959663602400/1017075323632448)
- [Python字符串操作手册](https://www.studytonight.com/python-string/)
- [Python标准库中的字符串处理模块](https://docs.python.org/3/library/string.html)