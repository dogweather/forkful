---
title:    "Haskell: 将字符串转换为小写"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## 为何

当我们处理字符串时，有时候需要将它们转换为小写，这样可以方便我们进行比较和匹配。Haskell提供了内置函数来帮助我们实现这一功能，让我们来看看如何使用它吧！

## 如何操作

首先，我们需要使用 `Data.Char` 模块来引入内置的 `toLower` 函数。

```Haskell
import Data.Char (toLower)
```

然后，我们可以通过将字符串作为函数的参数来调用 `toLower` 函数，从而得到转换后的小写字符串。

```Haskell
toLower "HELLO" -- 输出 "hello"
```

如果我们想要将字符串中的每个字符都转换为小写，可以使用 `map` 函数来进行操作。

```Haskell
map toLower "WORLD" -- 输出 "world"
```

## 深入探讨

需要注意的是，Haskell的字符串是由字符组成的列表，而不是传统意义上的字符串。因此，在使用 `toLower` 函数时，我们需要将其放在一个 `map` 函数中进行操作，因为 `toLower` 函数接受的是一个字符作为参数。

另外，Haskell的字符串是不可变的，这意味着当我们使用 `toLower` 函数进行转换时，原始字符串并不会被修改，而是返回一个新的字符串。

## 参考资料

- [Haskell官方文档](https://www.haskell.org/documentation/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)