---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么和为什么？

在编程中，提取子字符串是从特定的字符串中获取其中的一部分数据的过程。我们经常需要提取子字符串以获取并处理特定的信息。

## 如何做：

在 Haskell 中，我们可以使用 "take", "drop" 和 "splitAt" 函数来提取子字符串。让我们看一些例子：

```Haskell
main = do
  let str = "Haskell编程"
  print $ take 7 str
  print $ drop 7 str
  print $ splitAt 7 str
```

当我们运行上面的代码，结果是：

```Haskell
"Haskell"
"编程"
("Haskell", "编程")
```

## 深入研究

提取子字符串功能最早来源于传统的命令行文本处理工具，如 "cut" 和 "awk"。在Haskell中，"take" 和 "drop" 的实现详细依赖于惰性求值，只有在需要的时候才会求值。

对于提取子字符串的方法，我们也可以考虑使用其他的库，如 "Text.Regex"，以支持更复杂的字符串匹配和提取，但这通常需要更复杂的实现和额外的工作。

## 参考资料

Haskell substring extraction:
https://stackoverflow.com/questions/7869/substring-in-haskell

Haskell library string-functions:
https://hackage.haskell.org/package/string-functions

Haskell string manipulation examples:
https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/string