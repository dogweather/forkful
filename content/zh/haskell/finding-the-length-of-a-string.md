---
title:                "Haskell: 寻找字符串长度"
simple_title:         "寻找字符串长度"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么

字符串是计算机编程中常用的数据类型，因此计算字符串的长度是一项基本的技能。在Haskell中，我们可以通过一些简单的代码来获取字符串的长度。下面就让我们一起探讨一下如何实现这个功能吧！

## 如何进行计算

计算字符串的长度在Haskell中非常简单。我们只需要使用内置的`length`函数，它会返回给定字符串的长度。让我们来看一个简单的例子：

```Haskell
length "你好，世界！"   -- 输出：7
```

从上面的例子可以看出，字符串"你好，世界！"的长度为7个字符。让我们再来看一个例子：

```Haskell
length "Hello, World!"  -- 输出：13
```

在这个例子中，字符串"Hello, World!"的长度为13个字符。通过这个简单的函数，我们就可以轻松获取字符串的长度了。

## 深入挖掘

在Haskell中，字符串被定义为一个字符的列表。因此，计算字符串的长度实质上就是计算列表的长度。而`length`函数实际上就是使用递归的方式来计算列表的元素个数。如果你对递归感兴趣，可以尝试自己实现一个计算列表长度的函数。

## 参考资料

- [Haskell语言官方网站](https://www.haskell.org/)
- [Haskell函数资料](https://www.haskell.org/learn/books/#functions)
- [关于递归的详细解释](https://stackoverflow.com/questions/16153606/what-is-recursion)
- [Haskell标准库文档](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html)