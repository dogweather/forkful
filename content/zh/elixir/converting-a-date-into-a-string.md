---
title:                "将日期转换为字符串"
html_title:           "Elixir: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什么是日期转化字符串？为什么要这么做？
日期转化字符串是将日期数据转换为字符串格式的操作。程序员通常会这么做是因为字符串可以更容易地存储和处理日期数据，同时也可以使日期数据在不同的系统中更容易传递和显示。

## 如何实现：
```Elixir
date = ~D[2020-01-01]
date |> to_string |> IO.inspect
```
输出： "2020-01-01"

```Elixir
date = ~D[2020-01-01]
date |> to_string(format: "{YYYY-MM-dd}") |> IO.inspect
```
输出： "2020-01-01"

## 深入探讨：
日期转化字符串的历史可以追溯到计算机的早期发展，当时的计算机系统并没有直接处理日期数据的能力，因此程序员需要使用字符串来表示日期。现如今，大多数计算机系统都可以直接处理日期数据，但程序员仍然选择将日期转换为字符串来更方便地处理。除了使用```to_string```函数，还可以使用其他函数如```NaiveDateTime.to_iso8601```来实现日期转化字符串。

## 参考链接：
- [Elixir官方文档：Dates](https://hexdocs.pm/elixir/master/Dates.html)
- [关于日期转化字符串的更多信息](https://www.tutorialspoint.com/elixir/elixir_dates.htm)