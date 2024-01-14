---
title:    "Haskell: 计算字符串的长度"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

Mandarin translation:

# 为什么

字符串的长度是编程中经常要处理的一个问题。无论是统计文字数量，还是验证输入的合法性，获取字符串的长度都是必要的。在Haskell编程中，我们可以利用一个内置的函数来轻松地获取字符串的长度。在本文中，我将向大家介绍如何使用Haskell来获取字符串的长度，以及一些深入的讨论。

## 怎么做

在Haskell中，我们可以使用`length`函数来获取字符串的长度。下面是一个示例代码：

```Haskell
-- 定义一个字符串
str = "Hello World"

-- 使用length函数来获取字符串的长度
len = length str

-- 输出结果
print(len) -- 输出：11
```

我们也可以将`length`函数直接应用到字符串上，而不需要额外定义一个变量：

```Haskell
-- 直接获取字符串的长度
len = length "Welcome to Haskell"

-- 输出结果
print(len) -- 输出：20
```

虽然`length`函数可以直接返回字符串的长度，但是我们也可以使用模式匹配来获取。下面是一个示例代码：

```Haskell
-- 使用模式匹配来获取字符串的长度
strLength :: [a] -> Int
strLength [] = 0
strLength (x:xs) = 1 + strLength(xs)

-- 输出结果
print(strLength "This is a string") -- 输出：16
```

## 深入讨论

在Haskell中，字符串被看作是一个字符列表。因此，我们可以像处理列表一样来处理字符串。`length`函数也是基于此原理来计算字符串长度的。它会遍历每个字符，并计算字符的数量来获取字符串的长度。值得注意的是，`length`函数的时间复杂度为O(n)，其中n为字符串中的字符数量。

除了`length`函数外，Haskell还提供了其他一些方便的函数来处理字符串，例如`take`、`drop`、`reverse`等。通过深入研究这些函数，我们可以更加灵活和高效地处理字符串，使编程变得更加简单。

# 参考链接

- [Haskell Documentation: Strings](https://www.haskell.org/tutorial/strings.html)
- [Learn You a Haskell: Types and Typeclasses](http://learnyouahaskell.com/types-and-typeclasses)
- [Real World Haskell: Strings](http://book.realworldhaskell.org/read/strings.html)

# 参见

- [Haskell入门教程](https://zhuanlan.zhihu.com/p/37973060)
- [Haskell String Tutorial](https://www.tutorialspoint.com/haskell/haskell_string.htm)
- [Haskell Cheatsheet](https://cheatsheet.codeslower.com/CheatSheet.pdf)