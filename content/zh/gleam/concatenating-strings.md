---
title:    "Gleam: 连接字符串"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

为什么：连接字符串可以帮助我们在编程中更容易地构建和处理文本数据。例如，我们可以使用连接字符串来构建包含用户输入的句子，或者构建包含特定信息的数据格式。

如何操作：首先，我们需要使用```Gleam string concatenation```函数来连接字符串。以下是一个简单的例子：

```Gleam
let string1 = "Hello"
let string2 = "world"
let message = string1 ++ " " ++ string2
```

```Gleam
IO.println(message)
```

输出：

```
Hello world
```

深入了解：连接字符串实际上是将两个或多个字符串合并为一个字符串的过程。在Gleam中，我们可以使用++运算符来连接字符串，这个运算符可以用来连接任意数量的字符串。我们还可以使用字符串插值来将其他数据类型转换为字符串，并与其他字符串连接。

另外，我们还可以使用```Gleam string.split```函数来将字符串分割为多个部分，并使用连接字符串来重新构建不同的字符串组合。

参考链接：

[链接1：Gleam文档](https://gleam.run/standard-library-string.html#concatenation)

[链接2：Gleam string模块API文档](https://gleam.run/api-docs/#string)

[链接3：Gleam教程-字符串处理](https://gleam.run/tutorials/strings.html)

[链接4：Gleam官方博客-使用字符串插值](https://blog.gleam.run/posts/2020/06/30/gleam-tip-strings/)

查看也可见：

[进一步了解Gleam标准库中的字符串处理](https://gleam.run/standard-library-string.html)

[Gleam代码示例集锦-字符串处理](https://github.com/gleam-lang/gleam-by-example/blob/main/src/strings.gleam)

[Gleam社区讨论-字符串处理问题](https://github.com/gleam-lang/gleam/discussions/1755)