---
title:                "提取子字符串"
html_title:           "Gleam: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

Gleam 是一种简单易懂的编程语言，它提供了许多方便的工具来处理字符串，其中就包括提取子串。这个功能可以让你轻松地从一个较长的字符串中获取你所需要的部分，节省了手动处理字符串的时间和精力。不仅如此，它还能帮助你更加高效地处理数据，并且可以轻松地集成到其他项目中。

## 怎么做

要提取子串，首先需要使用" `substrings`"函数。这个函数接受两个参数，第一个参数是要提取子串的字符串，第二个参数是子串的起始位置和长度。以下是一个例子，演示如何从字符串"`Hello, World!`"中提取子串"`World`"：

```Gleam
import gleam/strings

let result = strings.substrings("Hello, World!", {start: 7, length: 5})

assert result == Ok("World")
```

可以看到，我们使用"`start`"参数来指定子串的起始位置，这里是第7个字符（索引从0开始），再使用"`length`"参数指定子串的长度，这里是5个字符。如果不想指定长度，也可以通过使用"`start`"和"`end`"参数来指定子串的终止位置，如下所示：

```Gleam
import gleam/strings

let result = strings.substrings("Hello, World!", {start: 7, end: 11})

assert result == Ok("World")
```

不仅如此，"`substrings`"函数还支持负数作为索引，可以从字符串的末尾往前提取子串。下面的例子演示如何从字符串"`Hello, World!`"中提取子串"`World!`"：

```Gleam
import gleam/strings

let result = strings.substrings("Hello, World!", {start: -6, length: 6})

assert result == Ok("World!")
```

当然，如果无法从给定的字符串中提取到子串，"`substrings`"函数会返回一个错误类型的结果。所以在使用之前，我们最好先进行错误处理。

## 深入探究

除了提取子串，Gleam还提供了其他许多处理字符串的函数，如"`trim`"、"`contains`"和"`replace`"等。同时，Gleam也提供了详细的文档和示例，方便开发者了解和使用这些函数。

## 进一步阅读

如果想了解更多关于字符串处理的内容，可以参考以下官方文档和示例：

- Gleam字符串处理文档：https://gleam.run/documentation/?redirected-from=stable.2.0%2Fdocs%2Ftypes%2Fstring
- Gleam字符串处理示例：https://github.com/gleam-lang/gleam/blob/stable.2.0/lib/strings/strings_test.gleam