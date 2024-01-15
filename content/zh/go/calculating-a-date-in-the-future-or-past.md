---
title:                "计算未来或过去的日期"
html_title:           "Go: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 为什么

很多时候我们需要计算未来或过去的日期，比如预定旅行日期或者计划会议安排。Go语言提供了方便的时间和日期计算功能，让我们不用手动计算，节省时间和精力。

## 如何做

下面是一个简单的示例代码，通过输入天数来计算未来或过去指定天数后的日期，并输出结果。请注意，代码中的日期格式遵循ISO 8601标准。

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// 今天的日期
	today := time.Now()
	fmt.Println("今天的日期是：", today.Format("2006-01-02"))

	// 计算未来10天后的日期
	future := today.AddDate(0, 0, 10)
	fmt.Println("未来10天的日期是：", future.Format("2006-01-02"))

	// 计算过去5天的日期
	past := today.AddDate(0, 0, -5)
	fmt.Println("过去5天的日期是：", past.Format("2006-01-02"))
}
```

运行上面的代码，将会得到以下结果：

```
今天的日期是： 2019-09-11
未来10天的日期是： 2019-09-21
过去5天的日期是： 2019-09-06
```

## 深入了解

除了上面的示例代码中提到的`AddDate()`方法，Go语言还提供了可以进行更精确计算的方法，比如`Add()`用于计算小时、分钟、秒等时间单位的增减，`AddDuration()`用于计算任意长度的时间增减。

如果你想要更深入了解Go语言中的时间和日期处理，可以参考官方文档中的[time包](https://golang.org/pkg/time/)和[日期和时间处理教程](https://gobyexample.com/time).

## 参考链接

- [Go官方文档：Time包](https://golang.org/pkg/time/)
- [日期和时间处理教程](https://gobyexample.com/time)
- [ISO 8601标准](https://en.wikipedia.org/wiki/ISO_8601)