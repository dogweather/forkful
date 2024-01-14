---
title:    "Elixir: 获取当前日期"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## 为什么

如果您正在使用Elixir编程语言，那么肯定会遇到需要获取当前日期的情况。获取当前日期在编程中是非常常见的需求，因为它可以帮助您跟踪数据和事件的发生时间。在本文中，我们将学习如何使用Elixir获取当前日期，并深入探讨一些细节。

## 如何

获取当前日期在Elixir中非常简单。您只需要使用`Date.utc_today`函数即可，它将返回一个当前日期的UTC时间戳。下面是一个简单的例子：

```Elixir
Date.utc_today
```

运行这段代码，你将得到类似这样的输出：

```
~U[2021-09-29 00:00:00]
```

这是一个UTC时间戳，它表示的是当前日期的零点。如果您想要自定义日期的格式，您可以使用`Date.format`函数。下面是一个例子：

```Elixir
Date.utc_today
|> Date.format("{0,Calendar} {0,Time}")
```

这将输出类似这样的结果：

```
2021-09-29 00:00:00
```

## 深入探讨

在Elixir中，日期是以`DateTime`数据类型表示的。它存储了日期和时间的信息，并且它是不可变的。当您使用`Date.utc_today`函数获取当前日期时，实际上是创建了一个`DateTime`对象，其中日期和时间信息都被设置为当前的UTC时间戳。

在编程的过程中，您可能会遇到需要将日期转换为不同的时区的情况。在Elixir中，您可以使用`DateTime.with_timezone`函数来实现这个功能。下面是一个例子：

```Elixir
DateTime.utc_now |> DateTime.with_timezone("Auckland")
```

这个例子将返回一个`DateTime`对象，其中日期和时间信息被转换为奥克兰的本地时间。

## 参考链接

- [Elixir Date module documentation](https://hexdocs.pm/elixir/Date.html)
- [DateTime module documentation](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir Date cheatsheet](https://devhints.io/elixir-date)
- [Working with dates and times in Elixir](https://medium.com/@vc143201/handling-dates-times-in-elixir-c64a9731b843)

## 参见

- [Elixir文档](https://elixir-lang.org/docs.html)
- [从零开始学习Elixir](https://elixirschool.com/zh-hans/)