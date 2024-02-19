---
aliases:
- /zh/elm/converting-a-string-to-lower-case/
date: 2024-01-20 17:38:48.738704-07:00
description: "\u4EC0\u4E48\u662F\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199\
  \uFF0C\u4EE5\u53CA\u4E3A\u4EC0\u4E48\u7A0B\u5E8F\u5458\u9700\u8981\u505A\u8FD9\u4E2A\
  \u64CD\u4F5C\uFF1F\u7B80\u5355\u6765\u8BF4\uFF0C\u5B57\u7B26\u4E32\u8F6C\u5C0F\u5199\
  \u5C31\u662F\u628A\u6587\u672C\u4E2D\u7684\u6240\u6709\u5927\u5199\u5B57\u6BCD\u6539\
  \u4E3A\u5C0F\u5199\u5F62\u5F0F\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u901A\u5E38\
  \u662F\u4E3A\u4E86\u6587\u672C\u6BD4\u8F83\u6216\u662F\u7EDF\u4E00\u6570\u636E\u683C\
  \u5F0F\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.047097
model: gpt-4-1106-preview
summary: "\u4EC0\u4E48\u662F\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199\
  \uFF0C\u4EE5\u53CA\u4E3A\u4EC0\u4E48\u7A0B\u5E8F\u5458\u9700\u8981\u505A\u8FD9\u4E2A\
  \u64CD\u4F5C\uFF1F\u7B80\u5355\u6765\u8BF4\uFF0C\u5B57\u7B26\u4E32\u8F6C\u5C0F\u5199\
  \u5C31\u662F\u628A\u6587\u672C\u4E2D\u7684\u6240\u6709\u5927\u5199\u5B57\u6BCD\u6539\
  \u4E3A\u5C0F\u5199\u5F62\u5F0F\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u901A\u5E38\
  \u662F\u4E3A\u4E86\u6587\u672C\u6BD4\u8F83\u6216\u662F\u7EDF\u4E00\u6570\u636E\u683C\
  \u5F0F\u3002"
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
---

{{< edit_this_page >}}

## What & Why?
什么是将字符串转换为小写，以及为什么程序员需要做这个操作？简单来说，字符串转小写就是把文本中的所有大写字母改为小写形式。程序员这么做通常是为了文本比较或是统一数据格式。

## How to:
在Elm中，你可以用 `String.toLower` 函数来把字符串转换为全小写。下面是个例子：

```Elm
import String

lowercaseString : String -> String
lowercaseString str =
    String.toLower str

-- 使用方法
result : String
result =
    lowercaseString "HeLLo, Elm!"

-- 输出 "hello, elm!"
```

在这段代码里，`lowercaseString` 是个接受任何字符串并将其转为小写的函数。用 `"HeLLo, Elm!"` 作为输入，输出将是 `"hello, elm!"`。

## Deep Dive
将字符串转换为小写是编程中很常见的需求，其历史可以追溯到计算机科学的早期，那时处理文本数据的方式比现在更为原始。在Elm中，`String.toLower` 函数用来做这个转换，它遵循当前语言环境（Locale）的标准规则，但要注意的是，Elm不支持指定语言环境。

除了`String.toLower`外，有时你可能需要对字符串进行更复杂的转化，比如只转换某些部分，或者对特定字符集做处理。在这些情况下，你可能需要使用正则表达式或者专门的字符串处理库。

Elm中的字符串转换实现比较直接，旨在提供高性能和可靠的结果。值得一提的是，这个标准函数并不涉及到何种存储或编码方式，它只关心字符如何被转换。

## See Also
- Elm官方文档关于字符串操作的部分: [String Module](https://package.elm-lang.org/packages/elm/core/latest/String)
- 关于Unicode和字符编码的更多信息: [Unicode Standard](http://www.unicode.org/standard/standard.html)
- 对字符串进行高级处理的Elm包，比如正则表达式: [Elm Regex](https://package.elm-lang.org/packages/elm/regex/latest)
