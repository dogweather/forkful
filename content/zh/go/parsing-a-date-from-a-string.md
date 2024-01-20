---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

解析日期从字符串是讲字符串形式的日期转化成具体的日期类型。程序员通常这么做，以便在代码中可以更方便有效的处理、操作和使用日期。

## 如何操作：

Go的`time`包提供了日期解析服务。使用`Parse`函数可以很容易实现日期解析。

```Go
package main
import (
    "fmt"
    "time"
)

func main() {
    dateStr := "2022-01-01"
    date, err := time.Parse("2006-01-02", dateStr)
    if err != nil {
        fmt.Println("日期解析出错：", err)
        return
    }
    fmt.Println("解析后的日期：", date)
}
```
样例输出:

```Go
解析后的日期： 2022-01-01 00:00:00 +0000 UTC
```

## 深入挖掘

历史背景：在早期，日期和时间的处理是最棘手的编程任务之一。不同的语言、操作系统甚至数据库在日期和时间的解析和表示方面各有差异。但是Go语言中的`time`包简化了日期和时间的处理。

替代方案：存在许多第三方库可以处理日期和时间，例如`jinzhu/now`，但原生`time`包已经足够应对大部分情况。

实现详情：`time.Parse`函数用于解析日期和时间。他的第一个参数是布局，它定义了给定字符串中日期和时间的格式。

## 另请参阅

- Go文档,`time`包：[https://golang.org/pkg/time/](https://golang.org/pkg/time/)
- `jinzhu/now`库：[https://github.com/jinzhu/now](https://github.com/jinzhu/now)
- Go日期和时间处理的更多信息：[https://yourbasic.org/golang/format-parse-string-time-date-example/](https://yourbasic.org/golang/format-parse-string-time-date-example/)