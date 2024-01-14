---
title:                "Arduino: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 为什么

计算未来或过去的日期在很多情况下都非常有用。比如，你可能想要创建一个计时器，或者需要知道某个事件是星期几。在这些情况下，使用Arduino编程来计算日期就是一个很好的解决方案。

## 怎么做

计算日期在Arduino编程中并不复杂。首先，你需要使用一个叫做 DateTime 的开源库。这个库可以帮助你操作日期和时间相关的函数。然后，你需要设置一个变量来储存你想要计算的日期。接下来，你可以使用 "```Arduino```" 代码块来编写你的计算代码。代码块中的内容如下：

```
#include <DateTime.h>  // 导入DateTime库 
DateTime currentDate (year, month, day);  // 设置变量存储日期
```

在这个代码块中，你需要用实际的日期来替换 year, month, day。例如，如果你想要计算 2021 年 10 月 10 日，那么你需要将代码改为 "DateTime currentDate (2021, 10, 10);"。

接下来，你可以使用 "```Arduino```" 代码块中的 DateTime 函数来计算未来或者过去的日期。以下是一个用于计算未来日期的例子：

```
DateTime futureDate = currentDate + TimeSpan(days, hours, minutes, seconds);
```

在这个例子中，你需要将 "days, hours, minutes, seconds" 替换为你想要计算的时间量。例如，如果你想要计算 10 天后的日期，那么你需要将代码改为 "DateTime futureDate = currentDate + TimeSpan(10, 0, 0, 0);"。同理，如果你想要计算过去日期，只需要将 "```+```" 替换为 "```-```"。

## 深入探讨

DateTime 函数中有很多可以探索的内容。例如，你可以使用 DateTime 的函数来获取当前时间、格式化日期和时间、比较日期等。除此之外，你还可以了解如何使用已知的日期来计算未知的日期，以及如何在不同的时区计算日期。

## 参考链接

- DateTime library: https://github.com/PaulStoffregen/DateTime
- 时间相关函数: https://www.arduino.cc/reference/zh/timing
- 时区相关函数: https://www.arduino.cc/reference/zh/timezone