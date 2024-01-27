---
title:                "字符串首字母大写"
date:                  2024-01-19
html_title:           "Arduino: 字符串首字母大写"
simple_title:         "字符串首字母大写"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
首先，将字符串大写就是将所有字符转成其对应的大写形式。程序员这么做是为了统一数据格式，或者在用户界面上强调某些文本。

## How to (如何操作)
```Gleam
import gleam/string

pub fn main() {
  let greeting = "hello, world!"
  let shout = string.uppercase(greeting)

  shout
}
```
输出：
```
"HELLO, WORLD!"
```

## Deep Dive (深入探讨)
字符串大写转换并不是一个新鲜事，这个功能在编程里面已经使用很多年了。Gleam中使用`string.uppercase`函数是最直接的方法。替代方案依赖于具体的编程环境，如在JavaScript中可以使用`toUpperCase()`。实现字符串大写的细节可能依赖于Unicode标准，因为处理不同语言中的特殊字符时需要特别注意。

## See Also (参考链接)
- Unicode标准: [Unicode's official website](http://www.unicode.org/)
