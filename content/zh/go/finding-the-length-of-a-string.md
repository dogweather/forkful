---
title:    "Go: 寻找字符串的长度"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么 ##
在编程中，字符串是一种非常常用的数据类型。我们经常需要确定字符串的长度，特别是在处理用户输入或者文件操作时。因此，学习如何找到字符串的长度是很有必要的，这样我们就可以更好地操作和处理字符串。

## 如何做 ##
在Go语言中，我们可以使用 `len()`函数来找到字符串的长度。以下是一个例子：
```Go
// 定义一个字符串
str := "你好，世界！"

// 使用len()函数找到字符串的长度
length := len(str)

// 打印结果
fmt.Println("字符串的长度为：", length)

// 输出：字符串的长度为： 7
```

从上面的例子中，我们可以看到使用`len()`函数是非常简单的。我们只需要在函数中传入字符串，函数就会返回字符串的长度。同时，我们还可以使用`fmt`包来打印结果，方便我们查看。

## 深入了解 ##
在Go语言中，字符串的长度指的是字符串包含的字节数。因此，如果我们的字符串中包含非ASCII字符（比如中文），那么每个字符会被转换成多个字节，从而影响字符串的长度。比如，上面的例子中，虽然字符串`"你好，世界！"`中有7个字符，但是由于每个中文字符占3个字节，所以字符串的长度为7*3=21个字节。

如果我们想要获取字符串中真正的字符数，可以使用`utf8.RuneCountInString()`函数。这个函数会计算字符串中的Unicode字符数量，比如上述例子中，`utf8.RuneCountInString()`函数会返回7，而不是21。

## 查看相关文章 ##
- [Go语言官方文档-字符串](https://golang.org/ref/spec#String_types)
- [Go语言标准库文档-utf8包](https://golang.org/pkg/utf8/)
- [Go语言中文网教程-字符串与字符](https://studygolang.com/articles/2968)