---
title:                "字串大写化"
html_title:           "Haskell: 字串大写化"
simple_title:         "字串大写化"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么要将字符串大写？
人们可能会想要将字符串大写，是因为在某些情况下，大写的字符串更方便阅读和处理，尤其是涉及到比较、搜索或者打印输出的时候。

## 如何实现？
在Haskell中，我们可以使用内置函数`toUpper`来实现字符串的大写操作，它将字符串中的所有小写字母转换为大写字母，并返回一个新的字符串。以下是一个简单的例子：
```Haskell
str = "hello world"
capitalizedStr = toUpper str
-- 输出："HELLO WORLD"
```

我们也可以自己实现一个简单的函数来将字符串大写，比如我们可以使用列表推导式和`toUpper`来遍历每个字符并将其转换为大写，然后再将字符列表组合成一个新的字符串，例如：
```Haskell
toUpperStr :: String -> String
toUpperStr str = [toUpper c | c <- str]
```
这个函数将接受一个字符串作为参数，并返回一个经过大写处理后的新字符串。

## 深入探讨
在Haskell中，字符串是一个由字符组成的列表，所以我们可以使用列表操作来对字符串进行操作。在`toUpperStr`函数中，我们使用了列表推导式来遍历每个字符并进行转换，还可以使用其他列表操作来实现一样的功能。除了`toUpper`之外，Haskell还提供了其他一些用于处理字符串的内置函数，如`toLower`用于将字符串转换为小写、`capitalize`用于将首字母大写等等。通过这些函数的组合，我们可以实现更复杂的字符串操作。

## 参考阅读
- [Haskell String Functions](https://www.tutorialspoint.com/haskell/haskell_string_functions.htm)
- [Learn You a Haskell - Strings](http://learnyouahaskell.com/starting-out#strings)
- [Real World Haskell - Strings](https://www.oualline.com/books.free/real.world/haskell-strings.html)
- [Hoogle - Haskell API Search](https://www.haskell.org/hoogle/?hoogle=string)
- [Haskell Wiki - Strings](https://wiki.haskell.org/Strings)