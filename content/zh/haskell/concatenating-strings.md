---
title:                "Haskell: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

#为什么

在任何编程语言中，字符串都是最基本的数据类型之一。拼接字符串是将多个字符串合并为一个单独的字符串的过程，这在日常编程中非常常见。拼接字符串可以帮助我们创建更复杂的文本，例如输出一条提示消息，或者创建一个完整的数据库查询语句。在Haskell中，我们可以使用一些简单的方法来进行字符串拼接，让我们来看看如何实现吧！

#如何做

首先，我们需要使用Haskell中的字符串拼接函数`++`。这个函数可以将两个字符串连接起来，形成一个新的字符串。让我们来看一个简单的例子：

```Haskell
"Hello " ++ "World"
```

这段代码将会输出`Hello World`。你可以看到，我们可以使用`++`符号来连接两个字符串。让我们再来看一个复杂一点的例子：

```Haskell
let name = "John"
let age = 25
let message = "Hello, my name is " ++ name ++ " and I am " ++ (show age) ++ " years old."
```

这段代码将会输出`Hello, my name is John and I am 25 years old.`。在这个例子中，我们还使用了Haskell中的`show`函数来将整数转换为字符串，这样我们就可以将年龄添加到我们的`message`字符串中。

#深入了解

在Haskell中，拼接字符串是非常高效的。因为在Haskell中，字符串其实就是一个字符列表，而`++`符号对于列表的连接是非常高效的。除了使用`++`函数，我们也可以使用`concat`函数来实现字符串的拼接。让我们来看一个例子：

```Haskell
concat ["Hello ", "World"]
```

这个例子与之前使用`++`符号拼接字符串的例子是等价的。

另外，你也可以使用字符串的列表来实现多个字符串的拼接，只需使用`concat`函数即可。例如：

```Haskell
let messages = ["Hello", "World", "!"]
concat messages
```

这段代码将会输出`Hello World!`。

#相关阅读

- [Haskell字符串操作教程](https://wiki.haskell.org/Strings)
- [Haskell列表操作教程](https://wiki.haskell.org/Lists)
- [Haskell函数式编程简介](https://www.haskell.org/)