---
title:                "计算未来或过去的日期"
date:                  2024-01-20T17:31:01.767771-07:00
model:                 gpt-4-1106-preview
simple_title:         "计算未来或过去的日期"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么?)
计算未来或过去的日期是指找出距离现在特定天数的日期。程序员这么做可能是为了处理到期日、预定日期或时间间隔。

## How to: (如何操作)
Go语言使用`time`包来计算日期。这里有个例子：

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// 当前时间
	now := time.Now()

	// 10天后
	tenDaysLater := now.AddDate(0, 0, 10)
	fmt.Println("十天后:", tenDaysLater.Format("2006-01-02"))

	// 3个月前
	threeMonthsBefore := now.AddDate(0, -3, 0)
	fmt.Println("三个月前:", threeMonthsBefore.Format("2006-01-02"))
}
```

输出可能会是这样：

```
十天后: 2023-02-28
三个月前: 2022-11-20
```

## Deep Dive (深入挖掘)
`time`包诞生于Go的早期版本, 现在是处理日期和时间的标准做法。有时候人们也用第三方库如`github.com/jinzhu/now`，但标准库已经足够好用。计算过去或未来日期时，`AddDate`方法接受年、月、日三个整数参数，负数代表过去，正数代表未来。

## See Also (另请参阅)
- Go官方文档中的`time`包: https://pkg.go.dev/time
- Go by Example中的时间处理: https://gobyexample.com/time
- Go 日期时间处理最佳实践: https://yourbasic.org/golang/time-change-date/