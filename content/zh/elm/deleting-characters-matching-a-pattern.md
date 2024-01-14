---
title:                "Elm: 匹配模式的字符删除"
simple_title:         "匹配模式的字符删除"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

##为什么
人们为什么要删除符合特定模式的字符呢？在Elm编程中，这种操作可以帮助我们更有效地处理数据，提高程序的运行速度，同时也能提高代码的可读性。

##如何操作
删除符合特定模式的字符是一种基本的字符串操作，在Elm中可以通过内置的函数来实现。例如，使用String.filter函数可以返回一个新的字符串，其中仅包含符合特定模式的字符。

```
Elm String.filter 示例代码：

let string = "HelloWorld"
let newString = String.filter (\ char -> char == "H") string
```

输出为 "H"

##深入了解
要在Elm中删除符合特定模式的字符，我们需要了解字符串操作的一些基础知识。首先，要知道Elm中的字符串是不可变的，这意味着不能直接修改已有的字符串，而是需要通过一些函数来创建新的字符串。

另外，在使用String.filter函数时，我们需要理解其参数类型为一个谓词函数（Predicate Function），即传入一个字符，返回一个布尔值。这样，在内部实现时，String.filter会遍历字符串，根据传入的谓词函数来决定是否将该字符加入新的字符串。这也是为什么我们需要传入一个匿名函数作为参数的原因。

##看看这些
如果想进一步了解Elm中的字符串操作，可以查看以下链接：

- Elm官方文档中关于字符串操作的介绍：https://elm-lang.org/docs/strings
- 知乎上的一篇关于Elm字符串操作的文章：https://zhuanlan.zhihu.com/p/79362111
- Stack Overflow上关于Elm中删除字符的讨论：https://stackoverflow.com/questions/44288693/in-elm-how-do-i-delete-a-character-in-a-string-at-a-specific-position

## 请参阅
- Elm字符串操作教程（英文）：https://elmprogramming.com/string-operators.html
- 张小龙的Elm入门教程（中文）：https://lightphoenix.me/post/2017-10-12-beginners-tour-of-elm-3-strings-and-booleans/