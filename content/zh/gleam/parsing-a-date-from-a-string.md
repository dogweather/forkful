---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
解析字符串中的日期是把字符串转换成日期形式。编程人员进行这个操作为了更有效地处理和比较日期。

## 如何操作：
你可以使用Gleam的内置函数来解析日期。以下是一个基础例子。

```
Gleam
import gleam/datetime
let date = datetime.from_iso8601("2021-06-01T19:32:17Z")
println(date)
```

你运行这个例子后，输出应该为：`Ok(#DateTime<2021-06-01T19:32:17Z>)`。

这个例子展示了如何使用Gleam的`datetime`库的`from_iso8601`函数来解析ISO 8601格式的日期和时间字符串。

## 深入探讨：
在计算早期，日期是以各种格式存储和表示的。为了标准化这一切，ISO 8601被创建出来，并且后来成为了解析日期中最常见的用法。

除了使用Gleam内置的函数以外，也有类似的库可以使用，例如`rustdatetime`。

实施日期解析有两个主要步骤。首先，函数读取并分析字符串中的每个字符。一旦发现匹配ISO 8601格式的字符序列，函数就会把它们装换成日期对象。

## 另请参阅：
1. Gleam官方文档的“DateTime”的部分：[https://hexdocs.pm/gleam_stdlib/gleam/datetime.html](https://hexdocs.pm/gleam_stdlib/gleam/datetime.html)。
2. 关于ISO 8601的详细信息：[https://en.wikipedia.org/wiki/ISO_8601](https://en.wikipedia.org/wiki/ISO_8601)。
3. 基于Rust的日期和时间库：[https://docs.rs/chrono/0.4.19/chrono/](https://docs.rs/chrono/0.4.19/chrono/)。