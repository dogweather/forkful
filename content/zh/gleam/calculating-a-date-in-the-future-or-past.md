---
title:                "Gleam: 计算未来或过去的日期"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 为什么要计算未来或过去的日期

有时我们需要计算未来或过去的日期，如在创建日历或计划活动时。使用Gleam编程语言可以轻松地完成这个任务。

## 如何进行计算
```Gleam
// 计算未来日期，参数为年、月、日
let future_date = Date.add(2021, 9, 20)

// 计算过去日期，参数为年、月、日
let past_date = Date.add(-3, 5, 14)

// 输出结果
IO.print("未来日期：${future_date}\n")
IO.print("过去日期：${past_date}\n")
```

以上代码将会输出如下结果：
```
未来日期：2021年9月20日
过去日期：2018年5月14日
```

## 深入探讨

在Gleam中，日期是一个结构体，它包含年、月、日等属性。我们可以使用Date.add函数来对日期进行计算，该函数接受年、月、日作为参数，并返回一个日期结构体。

此外，Gleam还提供了其他日期操作函数，如Date.subtract用于计算两个日期之间的差值，Date.compare用于比较两个日期的先后顺序等。

# 参考链接
- [Gleam官方文档](https://gleam.run/core/date/)
- [Gleam日期操作代码示例](https://github.com/gleam-lang/gleam/blob/main/examples/date/examples/README.md)
- [Gleam日期操作相关讨论](https://github.com/gleam-lang/gleam/issues/332)