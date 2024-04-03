---
date: 2024-01-20 17:45:29.345241-07:00
description: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u5C31\u662F\u4ECE\u4E00\u4E2A\u66F4\
  \u957F\u7684\u5B57\u7B26\u4E32\u4E2D\u62BD\u53D6\u4E00\u90E8\u5206\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u6837\u505A\u6765\u5206\u6790\u6587\u672C\uFF0C\u63D0\u53D6\u9700\u8981\
  \u7684\u4FE1\u606F\uFF0C\u6BD4\u5982\u7528\u6237\u8F93\u5165\uFF0C\u6587\u4EF6\u5185\
  \u5BB9\uFF0C\u6216\u7F51\u7EDC\u8BF7\u6C42\u6570\u636E\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.660375-06:00'
model: gpt-4-1106-preview
summary: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u5C31\u662F\u4ECE\u4E00\u4E2A\u66F4\
  \u957F\u7684\u5B57\u7B26\u4E32\u4E2D\u62BD\u53D6\u4E00\u90E8\u5206\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u6837\u505A\u6765\u5206\u6790\u6587\u672C\uFF0C\u63D0\u53D6\u9700\u8981\
  \u7684\u4FE1\u606F\uFF0C\u6BD4\u5982\u7528\u6237\u8F93\u5165\uFF0C\u6587\u4EF6\u5185\
  \u5BB9\uFF0C\u6216\u7F51\u7EDC\u8BF7\u6C42\u6570\u636E\u3002."
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
weight: 6
---

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
