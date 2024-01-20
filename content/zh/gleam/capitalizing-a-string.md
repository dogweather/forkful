---
title:                "将字符串大写化"
html_title:           "Gleam: 将字符串大写化"
simple_title:         "将字符串大写化"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 什么以及为什么？

大写字符串是指将字符串中的所有小写字母转为大写字母。编程开发者经常使用它来格式化输出或对比字符串，因为它能忽略字符串大小写的区分。

## 如何操作：

在Gleam中，你可以借助`to_upper`方法将字符串大写。下面是很直接的一个例子：

```gleam
import gleam/string.{from_utf8, to_utf8, to_upper}

fn main() {
  let str = "hello, gleam!"
  let capitalized = str |> to_utf8 |> to_upper |> from_utf8
  case capitalized {
    Ok(value) -> value
    Error(_) -> "Invalid string"
  }
}
```

输出会是：`"HELLO, GLEAM!"`

## 深入探讨

在大多数编程语言历史中，字符串的大写都是首选的格式化方法，并用于各种任务，如排序，过滤，和搜索等。

也许你可能会想，有其他方法可以大写字符串么？答案是肯定的。你可以手动遍历整个字符串，将每个小写字母转换为大写，但这显然不是最有效的办法。

`to_upper`函数的实现就是Gleam为我们提供了一种内置的方法来方便处理这一操作。它能通过二进制的形式处理字符串，并利用Erlang的字符串处理功能将其转换为大写。

## 另请参阅

1. Gleam文档, [Gleam string module](https://hexdocs.pm/gleam_stdlib/gleam/string.html#to_upper)
2. Erlang的大写字符串，[Erlang string to upper](http://erlang.org/doc/man/string.html#to_upper-1)