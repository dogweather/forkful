---
title:                "将日期转换为字符串"
html_title:           "Bash: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

将日期转换为字符串是一种数据处理手段，在 Go 语言中我们用 Time 包的 Format 方法。这种转化在很多开发场景下非常实用，比如：
1. 提供一种用户友好的显示方法
2. 方便进行时间戳的比较和排序

## 如何实现：

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    now := time.Now()
    fmt.Println(now.Format("2006-01-02 15:04:05"))
}
```
运行上述代码，你会看到类似下面的输出：
```Go
2022-03-09 14:01:36
```
我们成功地将当前的日期时间转化为字符串形式。

## 深入了解

1. 历史背景：Go语言的时间转换方法设计师取自于 Unix 的 date 命令，所以对于很多 Unix 用户来说，这种方法非常直观。
2. 替代方案：也可以使用Sprintf来进行日期到字符串的转换，但是Format方法可以灵活地控制日期格式。
3. 实现细节：Go语言的时间包(Time package)基于ISO 8601，这是国际标准的日期和时间的表示法。

## 还可以参考

- [Go文档中的Time包](https://pkg.go.dev/time)
- [Go语言示例：转换日期和时间](https://yourbasic.org/golang/format-parse-string-time-date-example/)
- [Stackoverflow的相关讨论](https://stackoverflow.com/questions/20234104/how-to-format-current-time-using-a-yyyymmddhhmmss-format)