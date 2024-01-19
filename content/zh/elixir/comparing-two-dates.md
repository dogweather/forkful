---
title:                "比较两个日期"
html_title:           "Clojure: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 是什么 & 为什么？
比较两个日期就是判断它们在时间上的先后顺序。编程者通常会进行日期比较以便于进行时间序列分析，进行排序，根据日期进行搜索或过滤等。

## 如何实现：
在 Elixir 中，我们可以使用 `Date.compare/2` API 来比较两个日期。下面是一个简单的例子。
```elixir
  iex> first_date = Date.from_iso8601("2020-01-01")
  {:ok, ~D[2020-01-01]}

  iex> second_date = Date.from_iso8601("2020-01-10")
  {:ok, ~D[2020-01-10]}

  iex> Date.compare(first_date, second_date)
  :lt  # 表示 first_date 小于 second_date
```

## 深入了解：
比较两个日期是编程语言中常见的功能，从早期的函数式编程语言到近期的面向对象编程语言，该功能一直被广泛应用。在Elixir中，`Date.compare/2` API 是用于比较日期的主要工具。然而，你也可以使用内置运算符 `>`，`<`，`>=`，`<=`，`==`，`!=` 来比较日期，当然，这需要你先确认一些条件。

比如说，我们要确保被比较的值一定是日期。同样，我们也需要注意时区的影响。由于 Elixir 支持原生时区处理，所以如果你的程序中涉及到时区，你需要使用正确的时区。

## 另请参阅：
如要深入了解 Elixir 中的日期函数，如 `Date.compare/2`，`DateTime.compare/2` 以及其它日期相关操作，你可以访问以下链接：
- [官方文档](https://hexdocs.pm/elixir/Date.html#compare/2)
- [Elixir School - Date & Time BASICs](https://elixirschool.com/en/lessons/basics/date_time/)
- [Elixir Forum - Working with dates](https://elixirforum.com/t/working-with-dates-times-and-timezones-in-elixir-a-brief-guide/19196)