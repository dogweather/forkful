---
title:                "获取当前日期"
date:                  2024-01-20T15:14:44.862327-07:00
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (是什么和为什么？)
在编程中获取当前日期意味着读取系统时钟显示现在的时间。程序员这么做是为了记录事件发生的时刻，设置定时器，或者给用户展示时间信息。

## How to: (如何操作：)
```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // 获取当前日期和时间
    now := time.Now()

    // 打印默认格式
    fmt.Println("现在时间:", now)

    // 按特定格式打印日期
    fmt.Println("格式化日期:", now.Format("2006-01-02 15:04:05"))
}
```
输出样例：
```
现在时间: 2023-04-12 17:06:15.123456789 +0800 CST m=+0.000000000
格式化日期: 2023-04-12 17:06:15
```

## Deep Dive (深入了解)
获取当前日期和时间是编程中的基础功能，Go 语言从诞生之初就内置了这一功能。在历史上，程序员依靠操作系统提供的服务来获取时间信息。实现细节方面，Go 使用了 `time` 包，该包内部与操作系统的时钟服务交互。

为何选择 `time.Now()`，而不是其它方式？首先，它简单易用。与其他一些语言相比，不需要手动创建日期对象。其次，`time.Now()` 返回的 `Time` 结构体广泛支持时间操作，如比较、增减时间等。

Go 标准库里的 `time` 包几乎能满足所有的时间处理需求。然而，如果您需要更复杂的时间处理，像处理时区转换，可以使用第三方库，如 `github.com/golang/time`。

## See Also (另请参阅)
- Go 语言官方文档中的 time 包：[Go time package](https://pkg.go.dev/time)
- 一篇深入分析 Go 里时间和日期处理的文章：[Go by Example: Time](https://gobyexample.com/time)
