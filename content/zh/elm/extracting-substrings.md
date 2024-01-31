---
title:                "提取子字符串"
date:                  2024-01-20T17:45:29.345241-07:00
model:                 gpt-4-1106-preview
simple_title:         "提取子字符串"

category:             "Elm"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
提取子字符串就是从一个更长的字符串中抽取一部分。程序员这样做来分析文本，提取需要的信息，比如用户输入，文件内容，或网络请求数据。

## How to: (如何执行：)
```Elm
import String exposing (slice)

-- 提取子字符串的例子

exampleString : String
exampleString = "Hello, Elm programmer!"

-- 从位置5开始到11结束（不包含11）
substring : String
substring = slice 5 11 exampleString

-- 输出结果: ", Elm"
```

## Deep Dive (深入探究)
从早期编程语言到现代Elm，提取子字符串一直是字符串操作的基础。Elm的`String.slice`函数是提取子字符串的基本手段。其他语言有各种方法，比如Python的索引切片或JavaScript的`substring()`和`slice()`方法。Elm在内部是如何实现这一功能的不太被外界所知，但这个功能建立在底层JavaScript的字符串处理能力之上，因为Elm最终会编译成JavaScript。

## See Also (另请参阅)
- Elm String module documentation: [https://package.elm-lang.org/packages/elm/core/latest/String#slice](https://package.elm-lang.org/packages/elm/core/latest/String#slice)
- "String processing in Functional Languages": 讨论函数式语言处理字符串方法的文章。
- Elm community discussions about string operations: [https://discourse.elm-lang.org/](https://discourse.elm-lang.org/)
