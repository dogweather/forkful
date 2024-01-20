---
title:                "使用正则表达式"
html_title:           "C: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
在编程中使用正则表达式进行文本搜索、替换、分析等任务。为快速高效处理字符串数据，编程中常用这个强大的工具。

## How to: (怎么做：)
在Gleam中，可以用标准库里的`regex`包处理正则表达式。以下是简单示例：

```gleam
import gleam/regex

pub fn demo() {
  let pattern = regex.regex("world").unwrap()
  regex.find(pattern, "Hello, world!")
}
```

输出为匹配结果，如下：

```gleam
Some(#(BitString(7..12), []))
```

## Deep Dive (深入了解)
正则表达式起源于20世纪50年代的神经生理学研究。今天，多种语言支持正则表达式，如Perl、Python、JavaScript。Gleam提供的`regex`库基于Rust的正则表达式引擎，性能高效。如果考虑性能或可读性，还可以用`String`模块的函数替代部分正则功能。

## See Also (另请参见)
- 正则表达式快速参考：[Regexr](https://regexr.com/)
- Rust正则表达式库文档：[Rust Regex](https://docs.rs/regex/)