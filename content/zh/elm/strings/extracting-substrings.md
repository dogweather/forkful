---
date: 2024-01-20 17:45:29.345241-07:00
description: null
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.978265-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u6267\u884C\uFF1A) \u4ECE\u65E9\u671F\u7F16\u7A0B\u8BED\u8A00\
  \u5230\u73B0\u4EE3Elm\uFF0C\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u4E00\u76F4\u662F\
  \u5B57\u7B26\u4E32\u64CD\u4F5C\u7684\u57FA\u7840\u3002Elm\u7684`String.slice`\u51FD\
  \u6570\u662F\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u7684\u57FA\u672C\u624B\u6BB5\u3002\
  \u5176\u4ED6\u8BED\u8A00\u6709\u5404\u79CD\u65B9\u6CD5\uFF0C\u6BD4\u5982Python\u7684\
  \u7D22\u5F15\u5207\u7247\u6216JavaScript\u7684`substring()`\u548C`slice()`\u65B9\
  \u6CD5\u3002Elm\u5728\u5185\u90E8\u662F\u5982\u4F55\u5B9E\u73B0\u8FD9\u4E00\u529F\
  \u80FD\u7684\u4E0D\u592A\u88AB\u5916\u754C\u6240\u77E5\uFF0C\u4F46\u8FD9\u4E2A\u529F\
  \u80FD\u5EFA\u7ACB\u5728\u5E95\u5C42JavaScript\u7684\u5B57\u7B26\u4E32\u5904\u7406\
  \u80FD\u529B\u4E4B\u4E0A\uFF0C\u56E0\u4E3AElm\u6700\u7EC8\u4F1A\u7F16\u8BD1\u6210\
  JavaScript\u3002"
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
weight: 6
---

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
