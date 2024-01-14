---
title:                "Gleam: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

#为什么要获取当前日期？
获取当前日期是为了在编程中使用日期相关的功能，例如计算过去的日期、比较不同日期之间的差距等等。在许多应用中，我们需要从系统中获取当前日期来完成特定的任务。

##如何实现
在Gleam编程语言中，我们可以使用标准库中的`os`模块来获取当前日期。下面的代码示例展示了如何使用该模块来获取当前日期，并打印出当前年、月、日的信息。
```Gleam
import os

let date = os.date()
os.log("当前年：{}", date.year)
os.log("当前月：{}", date.month)
os.log("当前日：{}", date.day)
```

运行以上代码，我们可以得到类似下面的输出：
```
当前年：2021
当前月：10
当前日：5
```

##深入探讨
在Gleam中，`os`模块实际上是对Erlang语言中的`:calendar`模块的封装。因此，我们可以通过查阅Erlang的文档来了解更多关于获取当前日期的方法和参数。例如，我们可以使用`os.date("YYYY/MM/DD")`的形式来指定日期的显示格式。

此外，在某些情况下，我们可能需要获取特定时区的当前日期。这时，我们可以使用`os.date("YYYY/MM/DD", {timezone: "东八区"})`来指定时区。

#参考资料
- [Gleam标准库文档 - `os`模块](https://gleam.run/docs/stdlib/os.html)
- [Erlang文档 - `:calendar.now/0`](http://erlang.org/doc/man/calendar.html#now-0)
- [Erlang文档 - `:calendar.now_to_local_time/1`](http://erlang.org/doc/man/calendar.html#now_to_local_time-1)

#另请参阅
- [如何使用Gleam编程语言获取当天日期](https://example.com/how-to-get-today-date-in-gleam)
- [Gleam官方网站](https://gleam.run)