---
title:                "Go: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么要获取当前日期？

获取当前日期对于编程来说是非常重要的。它可以帮助我们在日常应用中跟踪事件和记录时间，也可以作为程序中的一个重要参数。无论是在创建日志文件、调度任务还是生成报告，都需要对当前日期有所了解。

## 如何获取当前日期

在Go语言中，我们可以使用`time`包来获取当前日期。具体的代码如下：

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// 使用time包中的Now()方法来获取当前时间
	currentTime := time.Now()

	// 使用Format()方法来对日期进行格式化
	// 以下示例将日期格式化为"2006-01-02"的形式，具体格式化规则可以自行调整
	customFormat := currentTime.Format("2006-01-02")

	// 输出当前日期
	fmt.Println(customFormat)
}
```

运行以上代码，输出的结果将是当前日期的格式化形式，例如`2020-10-01`。通过调整`Format()`方法中的参数，可以实现不同的日期格式化。

## 深入了解获取当前日期

除了常规的日期格式化外，还有一些特殊的时间格式可以使用。例如，我们可以在日期字符串中加入时区信息，例如：

```Go
currentTime := time.Now()

// Format()方法中的"UTC"表示使用UTC时区
customFormat := currentTime.Format("Mon Jan 2 15:04:05 UTC -07 2006")
```

除了时区外，我们还可以对时间进行简单的操作，例如：

```Go
currentTime := time.Now()

// AddDate()方法可以在当前日期上增加一定的年月日
// 以下示例将当前日期增加1年
modifiedTime := currentTime.AddDate(1, 0, 0)

// 输出修改后的时间
fmt.Println(modifiedTime)
```

更多关于时间的操作，可以查看Go语言官方文档中的time包相关内容。

# 查看更多

- [Go语言官方文档 - Time](https://golang.org/pkg/time/)
- [Go语言中文网 - time包](https://studygolang.com/pkgdoc)
- [《Go语言圣经》 - 第十三章：并发](https://books.studygolang.com/gopl-zh/ch13/ch13-04.html)