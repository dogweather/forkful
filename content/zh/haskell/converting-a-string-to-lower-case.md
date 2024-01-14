---
title:                "Haskell: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 为什么要将字符串转换为小写

在Haskell编程中，字符串是一个常见的数据类型。有时候，我们可能需要将字符串中的所有字符都转换为小写，这样可以方便我们进行比较和处理。接下来，让我们一起来看看如何在Haskell中实现字符串转换为小写的功能吧！

## 如何操作

首先，我们需要导入Haskell的标准库Data.Char，这个库中包含了`toLower`这个函数，可以将一个字符转换为小写。然后，我们需要使用`map`函数来遍历字符串中的每一个字符，并将它们传入`toLower`函数中，最后再将所有字符组合起来，就可以得到转换为小写后的字符串了。

```Haskell
import Data.Char

toLowerString :: String -> String
toLowerString str = map toLower str

```

接下来，让我们看一下这段代码的运行结果吧！

```Haskell
toLowerString "Hello World"
```

输出结果为：

```
"hello world"
```

很简单吧！通过使用`toLower`函数和`map`函数，我们就能够将一个字符串转换为小写了。

## 深入探讨

除了上面提到的方法，我们还可以使用Haskell中的模式匹配来实现字符串转换为小写。具体操作是先将字符串分割成单个字符的列表，然后使用模式匹配来判断每个字符是否为大写字母，如果是，则将其转换为对应的小写字母，最后再将列表拼接成字符串。

```Haskell
import Data.Char

toLowerString :: String -> String
toLowerString "" = ""
toLowerString (x:xs)
  | x `elem` ['A'..'Z'] = toLower x : toLowerString xs
  | otherwise = x : toLowerString xs
```

在这个例子中，我们使用了空字符串和x:xs这两种模式来处理空字符串和含有多个字符的字符串。最后，让我们来看一下这段代码的执行结果吧！

```Haskell
toLowerString "Hello World"
```

输出结果为：

```
"hello world"
```

通过使用模式匹配，我们不仅可以将一个字符串转换为小写，还可以学习一种新的语法来处理复杂的字符串操作。

# 参考资料

- [Haskell标准库中的Data.Char模块](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html)
- [Haskell模式匹配指南](https://learn.hfm.io/pattern-matching.html)
- [Haskell for 语言参考中文版](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html)

# 参见

- [Haskell中字符串的常用操作](https://example.com/haskell-string-operations)
- [如何在Haskell中实现字符串比较](https://example.com/haskell-string-comparison)