---
title:                "Go: 查找字符串的长度"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 为什么要找到字符串的长度？

在编程中，字符串是一种非常常见的数据类型，它可以存储文本信息。但是，在处理字符串的时候，我们经常会遇到需要知道它的长度的情况。例如，我们可能需要检查用户输入的字符串是否符合要求，或者需要将字符串的长度作为条件来进行判断。因此，了解如何找到字符串的长度是非常重要的。

## 如何找到字符串的长度

在Go语言中，我们可以使用`len()`函数来获取字符串的长度。下面是一个简单的例子，展示了如何使用该函数来找到字符串的长度并打印出来。

```Go
str := "Hello World"
len := len(str) // 使用len()函数获取字符串的长度
println(len) // 打印出字符串的长度
```

运行这段代码，我们会得到输出结果 11，说明字符串"Hello World"的长度为11个字符。

## 深入了解

虽然在上面的例子中，我们已经成功找到了字符串的长度，但是你可能会好奇这个`len()`函数是如何工作的。其实，`len()`函数是Go语言的内置函数，它可以用来获取基础数据类型的长度，包括字符串、数组、切片、字典等。它的运行原理是遍历数据内部的元素，从而得到数据的长度。当然，Go语言内部也会对这些数据类型进行优化，使得`len()`函数可以更加高效地获取长度。

# 参考链接

- [Go语言中文网：字符串](https://studygolang.com/subject/2)
- [Go语言标准库文档：len()函数](https://golang.org/pkg/builtin/#len)
- [CSDN博客：关于Go语言字符串的长度](https://blog.csdn.net/u010947030/article/details/79331644)