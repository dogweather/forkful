---
title:                "搜索和替换文本"
date:                  2024-01-20T17:57:43.151649-07:00
model:                 gpt-4-1106-preview
simple_title:         "搜索和替换文本"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (什么和为什么？)
搜索与替换文本让我们可以快速修改代码或数据。程序员这么做以修复错误、更新信息或改进代码清晰度。

## How to (如何做)
在 Gleam 中，我们用标准库函数来搜索和替换。这里有个简单示例：

```gleam
import gleam/string

pub fn replace_example() {
  let text = "Hey there, old friend!"
  let new_text = string.replace(text, "old", "new")
  new_text
}
```

运行这段代码，输出会是：

```
"Hey there, new friend!"
```

`string.replace` 函数接受三个参数：原始文本、要搜索的文本和替换文本。

## Deep Dive (深入了解)
搜索和替换功能在文本编辑器的诞生时就存在了。它是基于正则表达式的概念，这种概念可以让我们进行复杂的文本模式匹配。在 Gleam 中，我们通常使用标准库函数，因为它们简单且类型安全。当然，如果需要更复杂的替换模式，我们还可以使用正则表达式库，比如 `gleam_regex`。

Gleam 语言关注类型安全和易读性，`string.replace` 正符合这个理念。但在一些古老的语言里，可能会使用更底层的函数来执行类似操作，有时候这些操作是不安全的，比如 C 语言的字符串操作函数。

## See Also (另见)
- 正则表达式基础: [Regular Expressions Info](https://www.regular-expressions.info/)