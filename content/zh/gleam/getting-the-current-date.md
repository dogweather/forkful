---
title:                "Gleam: 获取当前日期"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么要获取当前日期

在大多数软件开发中，我们经常需要获取当前的日期来进行数据记录、日期计算或者展示等操作。使用Gleam编程语言可以轻松地获取当前日期，并且可以根据自己的需求进行灵活的处理。

## 如何获取当前日期

获取当前日期的方法很简单，在Gleam中使用内置的Date模块即可。首先，我们需要导入Date模块：

```Gleam
import Date
```

然后，我们可以使用Date模块中的now函数来获取当前的日期，例如：

```Gleam
let today = Date.now()
```

默认情况下，now函数会返回一个日期对象，如下所示：

```Gleam
2021-08-18T00:00:00Z
```

如果我们想要格式化输出日期，可以使用Date模块中的format函数，例如：

```Gleam
let today = Date.now() |> Date.format("%Y/%m/%d")
```

上述代码会将日期格式化为年-月-日的格式，如下所示：

```Gleam
2021/08/18
```

除了使用format函数进行格式化，我们还可以通过Date模块中的其他函数来获取当前日期的具体信息，例如：

```Gleam
let year = Date.now() |> Date.getYear()
let month = Date.now() |> Date.getMonthName()
let day = Date.now() |> Date.getDayOfWeek()
```

上述代码会分别返回当前年份、月份和星期几。关于Date模块的更多用法，可以查阅官方文档。

## 深入了解获取当前日期

在Gleam中，日期对象是不可变的，也就是说，我们无法直接修改日期对象的值。如果我们想要进行日期计算，可以借助Date模块提供的函数来操作。例如，如果我们想要在当前日期的基础上加上一天，可以使用Date模块中的addDays函数，如下所示：

```Gleam
let tomorrow = Date.now() |> Date.addDays(1)
```

类似地，我们也可以使用subtractDays函数来减去指定的天数。除了日期计算，Date模块还提供了其他很多实用的函数，可以满足不同的需求。

# 查看更多

- 官方文档：https://gleam.run/core/Date.html
- Gleam编程语言官网：https://gleam.run/
- 在线交流社区：https://elixirforum.com/c/gleam