---
title:                "比较两个日期"
html_title:           "Elixir: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 为什么比较两个日期

许多时候，在编程中，我们需要比较两个日期的大小，以决定某些逻辑的执行。比如，我们可能需要检查某个活动是否已经过期，或者根据日期来显示特定的内容。在Elixir中，比较两个日期非常简单，让我们来看看如何做到这一点。

## 如何比较两个日期

在Elixir中，我们可以使用比较操作符（<、<=、>、>=）来比较两个日期的大小。让我们创建两个日期变量，并使用比较操作符来比较它们：

```elixir
date_1 = ~D[2020-01-01]
date_2 = ~D[2020-01-15]

IO.inspect(date_1 > date_2)
IO.inspect(date_1 < date_2)
IO.inspect(date_1 >= date_2)
IO.inspect(date_1 <= date_2)
```

上面的代码将输出以下结果：

```
false
true
false
true
```

我们也可以使用比较操作符来比较日期和时间的组合：

```elixir
date_3 = ~U[2020-01-01 12:00:00]
date_4 = ~U[2020-01-01 18:00:00]

IO.inspect(date_3 < date_4)
```

上面的代码将输出以下结果：

```
true
```

另外，我们还可以使用`==`和`!=`操作符来检查两个日期是否相等或不相等：

```elixir
date_5 = ~D[2020-01-01]
date_6 = ~D[2020-01-01]

IO.inspect(date_5 == date_6)
IO.inspect(date_5 != date_6)
```

上面的代码将输出以下结果：

```
true
false
```

## 深入了解比较日期

在Elixir中，日期和时间都是以秒为单位的整数，因此比较操作符直接比较这些整数值。如果我们想比较具有相同日期和时间精度的两个日期，我们可以使用`DateTime.compare/2`函数来比较它们：

```elixir
date_7 = ~U[2020-01-01 12:00:00]
date_8 = ~U[2020-01-01 18:00:00]

IO.inspect(DateTime.compare(date_7, date_8))
```

上面的代码将输出以下结果：

```
:lt
```

我们也可以使用`Calendar.compare/2`函数来比较日期和时间，它具有相同的行为：

```elixir
date_9 = ~D[2020-01-01]
date_10 = ~D[2020-01-02]

IO.inspect(Calendar.compare(date_9, date_10))
```

上面的代码将输出以下结果：

```
:lt
```

## 查看更多

- [官方Elixir文档: Date and Time types](https://hexdocs.pm/elixir/Date.html)
- [Elixir中的日期和时间操作](https://www.ludu.co/course/elixir/05-dates-and-time/02)
- [Elixir入门指南: 比较操作符](https://elixir-lang.org/getting-started/comparison-operations.html)