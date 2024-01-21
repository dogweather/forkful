---
title:                "提取子字符串"
date:                  2024-01-20T17:45:48.313888-07:00
model:                 gpt-4-1106-preview
simple_title:         "提取子字符串"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)

提取子字符串就是从一个字符串中抓取一部分内容。程序员这么做是为了分析或操作数据的片段，比如从一个日期中抓取年份。

## How to (如何操作)

在Gleam中提取子字符串使用的是`slice`函数。下面是如何使用的示例：

```gleam
import gleam/string

fn main() {
  let text = "Hello, world!"
  let world = string.slice(text, 7, 12)
  world
}
```

输出将是：

```
Ok("world")
```

## Deep Dive (深入了解)

历史上，提取子字符串这一需求促进了许多语言（包括Gleam）的字符串处理功能的发展。与其他语言相比，Gleam提供的`slice`函数返回的是`Result(String, String)`类型，确保了更安全的错误处理。除了`slice`，有时候其他函数如`split`或正则表达式也能完成相似的任务，但在不同的场景和用例中，每种方法的效率和便利程度都有所不同。在Gleam中关注安全和数据类型的严格性，可以借助错误处理的好处来减少运行时的问题。

## See Also (请参阅)

- Gleam官方文档关于字符串处理的部分：[https://gleam.run/book/tour/strings.html](https://gleam.run/book/tour/strings.html)