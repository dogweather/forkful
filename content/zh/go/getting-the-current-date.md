---
title:                "Go: 获取当前日期"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

为什么：获取当前日期的原因可能有很多。例如，程序需要根据当前日期来执行不同的操作，或者需要记录某项任务的完成时间。无论是什么原因，获取当前日期对于任何Go程序都是一个基本但重要的功能。 在本文中，我们将介绍如何用Go语言获取当前日期，并深入了解一些相关的知识。

## 为什么

获取当前日期的原因可能有很多。例如，程序需要根据当前日期来执行不同的操作，或者需要记录某项任务的完成时间。无论是什么原因，获取当前日期对于任何Go程序都是一个基本但重要的功能。

## 如何做

在Go语言中，我们可以使用time包来获取当前日期。首先，我们需要导入该包：

```
import "time"
```

接下来，我们可以使用`time.Now()`函数来获取当前日期和时间：

```
now := time.Now()
```

我们可以使用`now.Format()`函数来将日期和时间格式化为我们想要的字符串格式。例如，将日期格式化为`年-月-日`的格式：

```
today := now.Format("2006-01-02")
```

其中，`2006-01-02`是Go语言中规定的日期格式，可以根据自己的需要进行调整。完整的代码示例如下：

```
import "time"

func main() {
  now := time.Now()
  today := now.Format("2006-01-02")
  fmt.Println(today)
}
```

运行以上代码，我们可以得到类似于`2021-09-23`的日期。

## 深入了解

在深入了解获取当前日期的过程中，我们需要了解两个重要的概念：时区和时钟。

时区是指把地球分为不同区域，每个区域都有自己的标准时间。在Go语言中，我们可以使用`time.Location`来表示某个时区。默认情况下，Go程序运行的时区是UTC（协调世界时），我们可以使用`time.UTC`来表示它。

时钟是用来测量时间流逝的工具。在计算机中，时钟通常由一个计数器来模拟，每秒钟计数器都会增加一定的数值。在Go语言中，我们可以使用`time.Duration`来表示一段时间间隔（例如1秒），并使用`time.Ticker`来定时触发一些操作。

深入了解这些知识可以帮助我们更好地处理日期和时间相关的问题，并编写出更加有弹性的程序。

## 参考链接

- [time包文档](https://golang.org/pkg/time/)
- [Golang中的时区与时钟](https://liam.page/2020/05/13/timezone-clock-in-go/)

## 参见

- [如何在Go语言中执行定时任务](https://example.com)
- [Go语言中的日期计算技巧](https://example.com)