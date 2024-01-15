---
title:                "获取当前日期"
html_title:           "Go: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

你可能会想知道当前日期的原因有很多，可能是需要记录某个事件的起始时间，或者计算一段时间的持续时长，亦或是在程序中显示当天的日期等等。无论是哪种情况，获取当前日期都是一个基本的需求。在Go语言中，我们可以使用简单的代码来实现这一功能。

## 如何使用

在Go语言中，我们可以使用`time`包来处理时间和日期相关的功能。首先，我们需要导入这个包：

```
import "time"
```

接着，我们可以使用`time.Now()`函数来获取当前的日期和时间：

```
currentDate := time.Now()
```

通过这个函数，我们可以得到一个`Time`类型的变量`currentDate`，它包含有关当前日期和时间的信息。我们也可以通过`currentDate`变量来获取特定的日期和时间，比如：

```
currentYear := currentDate.Year()
currentMonth := currentDate.Month()
currentDay := currentDate.Day()
currentHour := currentDate.Hour()
currentMinute := currentDate.Minute()
currentSecond := currentDate.Second()

fmt.Println(currentYear, currentMonth, currentDay, currentHour, currentMinute, currentSecond)
```

这段代码的输出结果可能是：

```
2021 Mai 8 11 5 20
```

除了上述的日期和时间信息外，我们还可以通过`currentDate`变量来获取一周中的星期几，以及一年中的第几天：

```
currentWeekday := currentDate.Weekday()
currentDayOfYear := currentDate.YearDay()

fmt.Println(currentWeekday, currentDayOfYear)
```

这段代码的输出结果可能是：

```
6 128
```

## 深入了解

除了使用`time.Now()`函数来获取当前日期，我们还可以通过`time.Date()`函数来获取特定日期的信息。这个函数的参数依次为：年、月、日、时、分、秒、纳秒、时区。比如，我们可以通过以下代码来获取2020年5月1日的时间戳：

```
timestamp := time.Date(2020, 5, 1, 0, 0, 0, 0, time.Local)
```

除了上述的函数外，`time`包还提供了很多其他的有用功能，比如计算时间的差值、格式化时间、解析时间字符串等等。

## 参考链接

- [Go语言官方文档 - time](https://golang.org/pkg/time/)
- [Go语言程序设计（许式伟）](https://book.douban.com/subject/27044219/)
- [Go语言圣经（Alan A.A. Donovan, Brian W. Kernighan）](https://book.douban.com/subject/27044219/)

## 参见

- [Go语言日志打印入门指南](https://github.com/sunshineatnoon/Paper-Go-Log-From-Zero/)
- [了解更多关于Go语言的基础知识](https://github.com/Unknwon/the-way-to-go_ZH_CN/)