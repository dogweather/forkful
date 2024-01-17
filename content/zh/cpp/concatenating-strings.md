---
title:                "连接字符串"
html_title:           "C++: 连接字符串"
simple_title:         "连接字符串"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## 什么是字符串连接？

字符串连接是将两个或多个字符串合并为一个更长的字符串的过程。它是编程中常用的基本操作，可以将文本片段粘贴在一起，形成一个更完整的文本字符串。

## 为什么要做字符串连接？

程序员通常会通过字符串连接来构建更复杂的文本内容，这样可以更方便地操作文本数据。例如，在用户输入用户名和密码后，程序可以将它们连接起来，形成一个完整的登录凭证。另外，字符串连接也可以用来构造动态的文本消息，如发送电子邮件或短信。

## 如何进行字符串连接？

在C++中，可以使用加号 `+` 运算符来连接两个字符串。例如：

```C++
string str1 = "Hello";
string str2 = "World";
string result = str1 + str2;
```

此时 `result` 的值为 `HelloWorld`。注意，字符串连接只能在相同类型的字符串之间进行，即只能连接 `string` 型的字符串。

## 深入探讨

字符串连接的历史可以追溯到早期的编程语言，如BASIC。除了使用加号运算符，C++中还有其他方法来进行字符串连接，如使用 `string` 类中的 `append()` 方法。此外，还存在一些第三方库可供使用，如 `boost::algorithm::join()`，提供更多的灵活性和功能。在实现上，字符串连接涉及内存分配和复制，因此在处理大量字符串时，需要注意性能问题。

## 查看更多

- [C++ string concatenation](https://www.guru99.com/cpp-string.html)
- [A deep dive into string concatenation in C++](https://www.learncpp.com/cpp-tutorial/6-9a-the-string-class-concatenation-and-repetition/)
- [Boost string algorithms library](https://www.boost.org/doc/libs/1_72_0/doc/html/boost/algorithm/join.html)