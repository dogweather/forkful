---
title:                "寻找字符串的长度"
html_title:           "Haskell: 寻找字符串的长度"
simple_title:         "寻找字符串的长度"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么

为什么我们要去找出一个字符串的长度？因为在编程中，有时候需要知道一个字符串的长度来做一些操作，比如遍历和截取。

## 如何做

在Haskell中，我们可以使用内置函数`length`来查找字符串的长度。让我们来看一个例子：

```Haskell
str = "Hello World"  -- 定义一个字符串
len = length str    -- 使用length函数来查找长度
print(len)          -- 输出结果为 11
```

如果我们想要找出一个字符串中某个特定字符的长度，该怎么做呢？我们可以使用`filter`函数来进行筛选，然后再使用`length`函数来计算长度。如下所示：

```Haskell
str = "Hello World"
len = length (filter (== 'l') str)  -- 筛选出所有的l，然后计算长度
print(len)                          -- 输出结果为 3
```

## 深入探讨

在Haskell中，字符串实际上是由一个个Unicode字符组成的列表。因此，`length`函数实际上是在计算这个列表的长度，而不仅仅是字符串的字符数。

此外，我们还可以使用`map`函数来将字符串拆分成单个字符的列表，然后使用`length`函数来计算长度。如下所示：

```Haskell
str = "Hello World"
charList = map (:[]) str    -- 将字符串拆分为单个字符的列表
len = length charList       -- 计算列表的长度
print(len)                  -- 输出结果为 11
```

## 参考链接

- [Haskell 语言官方网站](https://www.haskell.org/)
- [Haskell Wiki](https://wiki.haskell.org/)
- [Haskell 函数文档](https://www.haskell.org/onlinereport/standard-prelude.html)