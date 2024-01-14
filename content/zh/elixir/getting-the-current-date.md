---
title:    "Elixir: 获取当前日期"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

#为什么
有时候我们需要知道当前的日期，比如在创建日历应用程序或记录时间戳时。Elixir提供了一种简单的方法来获取当前日期，让我们来看看如何做到这一点。

##如何
获取当前日期在Elixir中非常简单。我们可以使用内置的Date模块，并调用`utc_today`或`local_today`函数，具体取决于我们想要获取的是UTC时间还是本地时间。让我们一起来看一个例子：

```Elixir
# 获取UTC时间
Date.utc_today()

# 获取本地时间
Date.local_today()
```

输出将类似于下面的格式：

```
# 获取UTC时间
{:ok, #Date<2021-02-21>}

# 获取本地时间
{:ok, #Date<2021-02-21>}
```

我们也可以使用`DateTime`模块来获取更多的时间信息。让我们一起来看一个例子，获取当前时间的小时和分钟。

```Elixir
DateTime.utc_now() |> DateTime.to_time() |> Time.hour()
DateTime.utc_now() |> DateTime.to_time() |> Time.minute()
```

输出将类似于下面的格式：

```
12  # 当前时间的小时
37  # 当前时间的分钟
```

##深入了解
在Elixir中，日期实际上是一个`Date`结构体，它有三个字段：`year`、`month`和`day`。我们可以直接访问这些字段来获取特定的日期信息。例如，要获取当前日期的月份，我们可以这样写：

```Elixir
Date.utc_today().month
```

输出将是当前月份的数字，比如2月就是2。我们也可以通过调用`Date.to_string`函数来获取一个日期的可读字符串。让我们来看一个例子：

```Elixir
Date.utc_today() |> Date.to_string()
```

输出将类似于下面的格式：

```
"2021-02-21"
```

一个有用的技巧是使用`Date.add`函数来增加或减少日期的天数。让我们来看一个例子，在当前日期的基础上增加10天：

```Elixir
Date.utc_today() |> Date.add(10)
```

输出将是10天后的日期。我们也可以使用负数来减少日期。让我们来看一个例子，在当前日期的基础上减少5天：

```Elixir
Date.utc_today() |> Date.add(-5)
```

输出将是5天前的日期。

#参考
- [Elixir Date模块文档](https://hexdocs.pm/elixir/Date.html)
- [Elixir DateTime模块文档](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir Time模块文档](https://hexdocs.pm/elixir/Time.html)