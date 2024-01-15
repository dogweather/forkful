---
title:                "获取当前日期"
html_title:           "Elixir: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

有时我们需要在程序中获取当前的日期，这可以帮助我们做一些特定的处理，比如记录日志或者处理不同的数据。

## 如何实现

首先，在Elixir中，我们可以使用`Date.utc_today`函数来获取当前的UTC日期。这个函数会返回一个包含年、月和日的`Date`结构体。

```elixir
Date.utc_today()
#=> ~D[2022-02-28]
```

如果需要获取当前日期和时间，我们可以使用`DateTime.utc_now`函数。它会返回一个包含年、月、日、时、分、秒和毫秒的`DateTime`结构体。

```elixir
DateTime.utc_now()
#=> ~U[2022-02-28 12:34:56.789]
```

如果我们需要根据时区来获取当前日期和时间，可以使用`DateTime.from_naive/3`函数。

```elixir
DateTime.from_naive(DateTime.utc_now(), "Etc/UTC")
#=> ~U[2022-02-28 12:34:56.789Z]
```

## 深入了解

Elixir中内置的日期和时间模块非常强大，可以处理各种格式和时区的日期和时间。我们还可以使用`Calendar`模块来进行日期和时间的运算和转换，例如计算两个日期相差的天数、格式化日期和时间等。

## 参考链接

- [Elixir Date Module documentation](https://hexdocs.pm/elixir/Date.html)
- [Elixir DateTime Module documentation](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir Calendar Module documentation](https://hexdocs.pm/elixir/Calendar.html)