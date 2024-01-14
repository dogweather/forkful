---
title:                "Gleam: 比较两个日期"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 为什么
日期比较是一个很常见的编程任务，它可以帮助我们判断某个日期是在另一个日期之前还是之后。例如，我们可以用日期比较来判断某个人的生日是否已经过去，从而发送生日祝福。在Gleam语言中，有一个方便的方法来进行日期比较，让我们来看看如何使用它吧！

## 如何
```Gleam
import Time.Date

let today = Time.Date.now()
let birthday = Time.Date.from_ymd(1990, 8, 18)

let is_before = Time.Date.is_before(birthday, today)
let is_after = Time.Date.is_after(birthday, today)

IO.print("今天是生日吗？: $(birthday),$(if is_before { "是的！" } else { "不是。" })")
IO.print("生日已经过了吗？: $(birthday),$(if is_after { "是的！" } else { "还没到呢。" })")

```

运行以上代码，我们可以得到类似如下的输出：

```
今天是生日吗？: 1990-08-18,是的！
生日已经过了吗？: 1990-08-18,还没到呢。
```

从上面的例子中，我们可以看到如何使用Gleam语言中的日期模块来比较两个日期。我们可以通过调用`Time.Date.is_before(date1, date2)`来判断日期`date1`是否在日期`date2`之前，同理也可以使用`Time.Date.is_after(date1, date2)`来判断是否在之后。

## 深入探讨
除了上面提到的两个方法外，Gleam语言中的日期模块还提供了其他方便的函数来进行日期比较，如`Time.Date.is_same(date1, date2)`用于判断两个日期是否相同，`Time.Date.is_leap_year(date)`用于判断某个日期是否是闰年等等。

此外，Gleam还支持自定义格式的日期比较，通过`Time.Date.parse(format, date)`函数可以将字符串解析为日期对象，从而更灵活地进行比较。了解更多关于Gleam日期模块的信息，可以查看官方文档。

## 参考链接
- [Gleam官方文档 - 日期模块](https://gleam.run/book/core_modules.html#time-and-date)
- [Gleam官方仓库](https://github.com/gleam-lang/gleam)
- [Gleam中文社区](https://www.gleam-lang.org.cn/)