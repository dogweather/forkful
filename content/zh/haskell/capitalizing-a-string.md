---
title:                "将字符串转化为大写"
html_title:           "Haskell: 将字符串转化为大写"
simple_title:         "将字符串转化为大写"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么?

字符串大写是将字符串中的所有字母都转换为大写。程序员通常将其用于格式化输出，使其看起来更整洁，并且在一些大小写敏感的情况下，例如调试，也可能用到。

## 如何做:

Haskell中，你可以使用内置的 `Data.Char` 库的 `toUpper` 函数实现这一功能。下面是一个简单的示例:

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize = map toUpper

main :: IO()
main = do
    print $ capitalize "haskell is fun"
```

运行后的输出会是:

```Haskell
"HASKELL IS FUN"
```

## 深度探索

虽然大写字符串看起来是个小任务，但实际上这背后的实现有许多重要的考虑和细节。对于 `toUpper` 函数，它不仅仅是对 ASCII 字符集进行操作，它还对Unicode编码中多元化的大写/小写规则进行处理。

作为一种替代方法，你可以选择直接使用 ASCII 规则来进行转换，但是这可能会导致对非英文字符的处理出现问题。例如，"ü" 字符的大写形式仍然为 "Ü"，旧的ASCII规则可能无法处理此种情况。

Haskell的高阶函数也使编写大写转换函数变得更为简单和直观。我们使用 `map` 函数将 `toUpper` 应用到字符串中的每个字符，从而避免写出复杂且容易出错的循环。

## 参见

1. Haskell 中的 `Data.Char` 库文档: <https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html>
2. Haskell 中的 `map` 函数文档: <https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:map>
3. 关于 Unicode 大小写规则的更多信息: <http://www.unicode.org/versions/Unicode7.0.0/ch03.pdf>
4. 关于 Haskell 高阶函数的更多信息: <https://wiki.haskell.org/Higher_order_function>

以上简洁的介绍了在Haskell中如何进行字符串大写操作，其中涵盖了因实现中的技巧，替代方案以及相关的资源链接，希望能为你的学习提供参考。