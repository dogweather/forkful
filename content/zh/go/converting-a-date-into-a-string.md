---
title:                "Go: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么

对于Go程序员来说，将日期转换为字符串是一项常见的任务。它可以帮助我们将日期以特定的格式显示，例如在日志记录中显示时间戳，或者在用户界面中显示美观的日期。

## 如何进行

使用Go语言中的time包，我们可以轻松地将日期转换为字符串。首先，我们需要将日期存储到一个time.Time类型的变量中，然后使用time包中的Format函数来定义我们想要的日期格式。

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// 日期格式必须是"2006-01-02 15:04:05"这样的特定模式
	date := time.Date(2020, time.May, 15, 13, 30, 0, 0, time.UTC)

	// 使用Format函数将日期转换为字符串
	fmt.Println(date.Format("2006-01-02 15:04:05"))
	fmt.Println(date.Format("January 02, 2006"))
	fmt.Println(date.Format("3:04 PM"))
}
```

输出：
```
2020-05-15 13:30:00
May 15, 2020
1:30 PM
```

如上所示，我们可以根据自己的需求定义不同的日期格式，从而将日期转换为字符串。

## 深入探讨

在深入研究日期转换为字符串的过程中，我们需要了解一些关于time包的知识。首先，time.Time类型的变量可以表示从1970年1月1日到现在的时间差，也就是类似于Unix时间戳。此外，我们还需要知道Format函数中使用的日期格式必须严格按照"2006-01-02 15:04:05"这样的模式，它是为了尊重Go语言的创建时间，也就是2006年1月2日15点04分05秒。

## 看看这些

- [Go语言中的时间和日期](https://studygolang.com/articles/2055)
- [Go语言官方文档-时间包](https://golang.org/pkg/time/)
- [介绍time包中的Format函数](https://yourbasic.org/golang/format-parse-time-date-go/)