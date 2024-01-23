---
title:                "获取字符串的长度"
date:                  2024-01-20T17:47:21.419750-07:00
model:                 gpt-4-1106-preview
simple_title:         "获取字符串的长度"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (什么及为什么?)
字符串长度就是字符的数量。程序员需要知道它来处理文本数据，如验证输入或限制长度。

## How to: (如何做)
在Gleam中, 你可以用 `String.len` 函数来找到一个字符串的长度。例子如下：

```gleam
import gleam/io
import gleam/string

pub fn main() {
  let greeting = "你好，世界！"
  let length = string.len(greeting)
  io.debug(length) // 将打印：6
}
```
注意：在这里，“你好，世界！”的长度是6，因为Gleam计算UTF-8编码的字符个数。

## Deep Dive (深入研究)
字符串长度的计算取决于编码。早期，ASCII编码的字符串一个字符占一个字节，长度计算简单。但现在，由于UTF-8编码，一个字符可能由多个字节构成，复杂化了长度计算。Gleam使用Unicode标量值计算字符串长度。

备选方案包括直接遍历字符串的字节来计算长度，这在某些编程语言中是必要的，因为它们没有内建函数来计算编码后的字符串长度。

在Gleam中，`String.len` 是一个内置函数，它遵循Erlang的底层实现。它等同于Erlang的 `unicode:characters_to_nfc_binary/1` 函数，这确保了字符串长度的准确且高效的计算。

## See Also (参考链接)
- 如果你对Erlang的字符串处理方法感兴趣，可以这里查看：[Erlang -- unicode](http://erlang.org/doc/man/unicode.html)
