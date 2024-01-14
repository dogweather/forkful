---
title:                "Haskell: 将字符串大写"
simple_title:         "将字符串大写"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在编写Haskell代码时，有时候我们需要将字符串的首字母变成大写。这可能是因为代码的规范要求，或者是为了使得输出更美观。

## 如何做到

要实现这一功能，我们可以使用Haskell标准库中的toTitle函数。这个函数接受一个字符串作为参数，并返回一个新的字符串，其中首字母变成大写，其他字母保持不变。

```Haskell
import Data.Char (toTitle)

capitalize :: String -> String
capitalize str = toTitle (head str) : tail str
```

下面是一个使用例子，我们将字符串"haskell"变成"Haskell"。

```Haskell
main :: IO ()
main = putStrLn (capitalize "haskell")
```

输出结果为："Haskell"

## 深入探讨

首先要注意的是，toTitle函数只会将ascii字符转换成大写，其他字符不会改变。所以如果想要将一个字符串的每个单词的首字母都变成大写，我们需要对字符串进行拆分，然后再分别处理每个单词。

此外，toTitle函数也支持Unicode字符的转换。但是要注意，不同的编码系统可能会有不同的行为，所以在使用toTitle函数时，最好先了解一下所使用的编码系统的特点。

## 参考资料

[Data.Char - Hackage](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html)

[探究Haskell字符串的编码问题](https://www.cnblogs.com/lazybug/p/6976726.html)

# 参见

[Markdown入门指南](https://www.zhihu.com/question/19963642)

[Haskell中文编程指南](https://wiki.haskell.org.cn/Haskell%E9%A6%96%E9%A1%B5)