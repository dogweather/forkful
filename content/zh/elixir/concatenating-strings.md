---
title:                "拼接字符串"
html_title:           "Elixir: 拼接字符串"
simple_title:         "拼接字符串"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

从字面上理解，连接字符串就是将两个或多个字符串合并成一个新的字符串。程序员经常这样做是因为需要动态地构建特定的字符串，比如打印日志信息、生成文件路径等等。

## 如何操作：

使用Elixir编程语言可以轻松地连接字符串。以下示例展示了如何使用加号和String.concat/2函数来连接两个字符串，并打印出结果。
```
Elixir
string1 = "Hello"
string2 = "World"
result = string1 <> " " <> string2
IO.puts(result)
```

输出结果：
```
Hello World
```

## 深入了解：

字符串连接有着悠久的历史，在早期的编程语言中就已经存在。除了使用加号和String.concat/2函数外，还有其他的实现方法，比如使用String.join/2函数、IO.write函数等。通过深入探讨这些不同的实现方式，可以帮助我们更好地理解实现原理，并选择适合自己的方法。

## 查看更多：

想了解更多关于字符串连接的知识，请参考以下资源：

- Elixir官方文档：https://hexdocs.pm/elixir/String.html#concat/2
- 《Elixir-言简意赅》一书中关于字符串操作的章节：https://ileigar.com/book/code/index.html#elixir_1
- Stack Overflow上与字符串连接相关的问题和答案：https://stackoverflow.com/questions/tagged/elixir+string+concatenation