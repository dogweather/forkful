---
title:                "匹配模式删除字符"
date:                  2024-01-20T17:42:22.336562-07:00
model:                 gpt-4-1106-preview
simple_title:         "匹配模式删除字符"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
删除匹配模式的字符是指在文本串中识别并移除那些符合特定规则的字符。程序员这样做通常是为了数据清洗、格式化输出，或满足语言处理的要求。

## How to: (如何操作:)
```gleam
import gleam/string

fn remove_pattern(text: String, pattern: String) -> String {
  string.replace(text, pattern, "", -1)
}

pub fn main() {
  let text = "Hello, 世界! 123-456-7890"
  let cleaned_text = remove_pattern(text, "\\d|-")
  assert cleaned_text == "Hello, 世界! "
}
```
输出：
```
Hello, 世界! 
```

## Deep Dive (深入探究)
从历史角度看，字符匹配删除这一概念源自早期Unix文本处理工具，如sed和awk。Gleam语言提供了现代化的字符串处理函数，如`string.replace`，这取代了原始的正则表达式工具。相较于直接用正则库，使用Gleam的内置函数通常更简单、类型更安全。实现细节方面，`string.replace`函数在底层可能使用了高效的字符串搜索和匹配算法，如KMP（Knuth-Morris-Pratt）。

## See Also (另请参阅)
- Gleam 入门: https://gleam.run/book/
- Gleam 字符串模块文档: https://hexdocs.pm/gleam_stdlib/gleam/string/
- 正则表达式教程: https://www.regular-expressions.info/tutorial.html