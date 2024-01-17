---
title:                "提取子字符串"
html_title:           "Gleam: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么是子字符串 & 为什么要提取它？
子字符串是一个字符串的一部分。程序员们经常提取子字符串来获取特定的信息或者操作字符串。它是一种简单而有效的方法，可以帮助我们处理字符串，让我们的程序更加灵活和可读性更强。

## 如何操作：
```Gleam
str = "Hello, world!"

// 提取 "world"
substring = str.slice(7, 12)

// 输出 "world"
io.println(substring)
```

## 深入了解：
在过去，程序员们通常会使用手动的方法来提取子字符串，比如使用for循环来遍历字符串并复制所需的部分。但现在，提取子字符串已经变得更加简单，因为我们可以使用库函数来实现。Gleam中的`slice`函数就是一个很好的例子，它可以在指定的位置提取出指定数量的字符。

除了使用`slice`函数之外，我们还可以使用正则表达式来提取子字符串。这种方法适用于需要在复杂模式中提取字符串的情况，比如在处理网页内容时。

当然，我们也可以编写自己的函数来提取子字符串，但这可能会更加麻烦和容易出错。因此，建议尽可能使用现有的库函数来处理。

## 相关资源：
- [Gleam文档 - 字符串操作](https://gleam.run/documentation/library/#string)
- [正则表达式教程](https://www.runoob.com/regexp/regexp-intro.html)
- [Gleam正则表达式库](https://github.com/gleam-lang/gleam/blob/master/std/re/lib/regexp_library.gleam)