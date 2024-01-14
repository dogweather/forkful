---
title:    "Gleam: 获取当前日期"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

为什么获取当前日期

获取当前日期可能是在您的程序中非常重要的一个步骤。通过获取当前日期，您可以确保程序中使用的时间是最新的，从而避免出现错误或混乱。

## 如何获取当前日期

在Gleam中获取当前日期非常简单。您只需要使用内置的DateTime模块中的函数就可以获取到当前日期的信息。

```Gleam
import gleam/datetime

let current_date = datetime.now()

// 输出例子：2021年10月3日，星期日
datetime.format(current_date, "%Y年%m月%d日，%A")
```

现在您可以根据您的需求，使用不同的格式来获取当前日期，例如，如果您只需要年月日，可以使用如下代码：

```Gleam
import gleam/datetime

let current_date = datetime.now()

// 输出例子：2021-10-03
datetime.format(current_date, "%Y-%m-%d")
```

## 深入探讨获取当前日期

在Gleam中，DateTime模块提供了许多有用的函数来处理日期和时间。除了获取当前日期外，您还可以使用其他函数来进行日期的比较、转换以及格式化等操作。通过深入学习DateTime模块，您可以更加灵活地处理日期和时间，从而提高程序的效率。

## 参考资料

- [Gleam官方文档 - DateTime模块](https://gleam.run/documentation/standard-libraries/datetime)
- [使用Gleam来处理日期和时间](https://medium.com/gleam-lang/using-gleam-to-handle-dates-and-times-3879f0110c44)
- [Gleam社区论坛](https://forum.gleam.run/) 

## 参见

- [如何在Gleam中获取当前时间](https://github.com/user/guide-how-to-get-current-time)
- [使用Gleam处理日期和时间的最佳实践](https://github.com/user/best-practices-for-handling-dates-and-times-in-gleam)