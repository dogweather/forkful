---
title:                "字符串拼接"
date:                  2024-01-20T17:34:46.258435-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串拼接"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? / 什么以及为什么？
串接字符串就是将两个或多个字符串拼成一个。我们这样做是为了创建更复杂的文本数据，比如拼出完整的句子或动态生成的内容。

## How to: / 如何做：
Gleam 语言里串接字符串简单直接。看看下面的例子：

```gleam
pub fn main() {
  let greeting = "你好, "
  let name = "世界!"
  let message = greeting ++ name
  message
}
```

输出将会是：
```
你好, 世界!
```

## Deep Dive / 深入探究
在早期编程语言中，串接字符串可能会很复杂，因为要考虑内存管理等问题。现在，在像 Gleam 这样的现代语言里，这变得轻而易举。Gleam 在内部使用的 Rust 语言，帮助它有效地处理字符串操作。替代方法有很多，比如使用格式化宏或构建器模式，但简单串接经常是最直接的方法。还要注意的是，大量串接操作可能会影响性能，因为每次串接都可能产生新的字符串。

## See Also / 另请参阅
- Gleam Documentation on Strings: [https://gleam.run/book/tour/strings.html](https://gleam.run/book/tour/strings.html)
- Rust Documentation on Strings: [https://doc.rust-lang.org/book/ch08-02-strings.html](https://doc.rust-lang.org/book/ch08-02-strings.html) (Helpful because Gleam compiles to Rust)