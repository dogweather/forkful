---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么与为什么？
获取当前日期可以让我们在编程中知道现在的时间。程序员通常需要这个信息来追踪事件，记录日志，或用来生成动态内容。

## 如何做：
在 Go 中获取当前日期的代码如下：

```Go
package main
import (
   "fmt"
   "time"
)

func main() {
   // Get current date
   current_time := time.Now()
   fmt.Println("Current Date and Time is: ", current_time.String())
}
```
执行这段代码，输出像这样：
```
Current Date and Time is:  2022-02-22 11:51:38.766157 +0000 UTC m=+0.000000001
```
## 深度学习
历史上，程序员通过许多不同的方法获取当前日期。例如在 Unix 系统中，我们可以调用 `date` 命令。而在 Go 中，我们使用 `time` 包来获取日期。

然而，有许多其他方案可以实现这个功能。你可以调用操作系统的功能，比如调用 C 语言的 `time() `函数。或者，也可以通过网络获取，比如使用 NTP 服务器。

Go 的 `time` 包包含许多有用的功能，可以让你获取、解析和格式化日期。`time.Now()`函数获取当前时间，返回的是一个 `Time` 对象，我们用 `String()`方法将其转化为人类可读的形式。

## 另请参阅
以下是一些你可能会感兴趣的链接，它们提供了关于 Go 日期和时间处理的更多信息：
1. 时间和日期：https://golang.org/pkg/time/
2. Go 的时间格式化：https://gobyexample.com/time-formatting-parsing
3. more about Go: https://tour.golang.org/