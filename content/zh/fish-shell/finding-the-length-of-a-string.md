---
title:                "Fish Shell: 找到字符串的长度"
simple_title:         "找到字符串的长度"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 为什么？
字符串的长度是我们经常会遇到的一个问题。它可以用来确定字符串中有多少个字符，或者在编程中需要用到限定字符串长度的情况下，可以让我们更方便地控制代码。

## 如何
在Fish Shell中，要找到字符串长度很简单。只需要使用`string length`命令即可。下面是一个例子：

```Fish Shell
set str "Hello, world!"
echo (string length $str)
```

输出会是`13`，因为字符串中有13个字符（包括空格和标点符号）。你也可以使用字符串变量来代替直接写入字符串，让代码更加灵活。另外，如果想要程序只返回字符串的长度，而不是打印出来，可以使用`string length --print 0`命令。

## 深入探讨
在Fish Shell中，`string length`命令使用的实际上是`count`函数。它使用的是Unicode编码来计算字符串的长度，而不是字节数。这意味着在使用含有特殊字符的字符串时，也可以得到正确的长度。同时，`string length`命令也支持多行字符串，它会将所有行都算在内，包括行尾的换行符。

# 查看更多
## 参考链接
- [Fish Shell官方文档：string length命令](https://fishshell.com/docs/current/cmds/string.html#length)
- [教程：掌握Fish Shell中的字符串处理](https://blog.fishshell.com/mastering-string-handling-in-fish-shell/)
- [知乎：如何在Fish Shell中处理字符串？](https://www.zhihu.com/question/62202860)