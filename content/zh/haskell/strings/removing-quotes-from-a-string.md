---
title:                "从字符串中移除引号"
aliases:
- /zh/haskell/removing-quotes-from-a-string/
date:                  2024-01-26T03:39:55.387232-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串中移除引号"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
从字符串中移除引号意味着剥离字符串数据部分的任何引号标记——单引号（' '）或双引号（" "）。程序员这样做是为了对输入进行消毒，准备文本以便处理，或者去除可能干扰数据处理和操作的不必要字符。

## 如何操作：
在Haskell中，我们可以编写一个函数，用于删除给定字符串中的所有引号。这就像是告诉引号走开，并确保它们明白这个暗示。

```Haskell
import Data.List (intercalate)
import Data.Char (isPunctuation)

removeQuotes :: String -> String
removeQuotes = filter (\c -> c /= '"' && c /= '\'')

main :: IO ()
main = do
    let stringWithQuotes = "Haskell said, \"Let's learn some functions!\""
    putStrLn $ removeQuotes stringWithQuotes
```

示例输出：

```
Haskell said, Lets learn some functions!
```

## 深入探讨
曾几何时，在编程中的字符串像互联网上的猫视频一样普遍之前，处理文本是一件棘手的事情。但随着编程语言的发展，字符串成为了编码的一个关键部分。然而，引号仍然是一把双刃剑——对于定义字符串来说是必不可少的，但作为实际数据包含在内时则是一种麻烦。

有其他方法吗？你可以选择性地行动，而不是像赶苍蝇一样拍打所有的引号。你可能希望只移除最外层的引号（一种典型的修剪）或处理字符串内的转义引号。

在实现上，上述的`removeQuotes`函数使用一个lambda来检查每个字符（`c`），看它是否是一个讨厌的引号，并据此过滤它们。这是一种直接的方法，但对于更大的文本或更复杂的规则，你可能会希望查看像`Parsec`这样的解析库，它们在文本处理中可以给你更多的细腻和能力。

## 另见：
- 对于正则表达式爱好者：[Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix)
- Haskell字符串的温和介绍：[Learn You a Haskell for Great Good! - Starting Out](http://learnyouahaskell.com/starting-out#strings)
