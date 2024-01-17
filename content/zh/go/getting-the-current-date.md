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

# What & Why?: 
获取当前日期是指在程序中获取当前的日期信息。程序员这样做的原因是因为在很多应用中，需要使用到当前日期来进行计算或者显示给用户。

## How to:
使用`Go time`库来获取当前日期非常简单。只需使用`time.Now()`函数即可。以下是一个示例代码和输出：

```Go
package main

import (
  "fmt"
  "time"
)

func main() {
  // 使用`Now()`函数来获取当前日期
  currentDate := time.Now()

  // 使用`Format()`函数来格式化输出
  fmt.Println("当前日期：", currentDate.Format("2006-01-02"))
}
```

输出：

```
当前日期： 2021-07-26
```

## Deep Dive:
获取当前日期在程序中非常常见，因此很多语言都提供了对应的库或函数来实现。在早期，程序员只能通过系统的日期函数来获取当前日期，这样并不方便也不灵活。随着编程语言的发展，越来越多的语言都提供了专门处理日期和时间的库，比如Java中的`java.time`库和Python中的`datetime`库。在Go语言中，使用`time`库来获取当前日期非常方便，而且可以通过调用其它函数来获取更多的日期信息，比如年份、月份、星期等等。

## See Also:
- [Go时间和日期文档](https://pkg.go.dev/time)
- [Java中的日期和时间文档](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Python中的日期和时间文档](https://docs.python.org/3/library/datetime.html)