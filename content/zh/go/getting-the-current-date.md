---
title:    "Go: 获取当前日期"
keywords: ["Go"]
---

{{< edit_this_page >}}

# 为什么：获取当前日期的作用

在编程中，经常需要获取当前日期，以便进行时间相关的操作，如记录日志、计算日期间隔等。获取当前日期可以让程序更加灵活和精确地处理时间问题。

# 如何获取当前日期

使用Go语言内置的time包可以轻松获取当前日期。下面是一个简单的示例代码，展示如何使用time包中的Now()函数来获取当前日期，并将其转换为字符串格式输出。

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // 获取当前日期并转换为字符串格式
    currentDate := time.Now().String()
    
    // 输出结果
    fmt.Println("当前日期：", currentDate)
}
```

输出结果：

```
当前日期：2021-01-01 15:00:00 +0800 CST
```

# 深入了解获取当前日期

除了使用Now()函数，time包还提供了许多其他有用的函数来获取当前日期和时间。例如，使用Now()函数的Local()方法可以获取当前的本地时间，使用Now()函数的UTC()方法可以获取当前的UTC时间。此外，time包还提供了格式化日期和时间的方法，让程序可以根据自己的需求来显示日期和时间。

# 参考链接

- [Go语言官方文档：time包](https://golang.org/pkg/time/)
- [Go语言中文文档：time包](https://studygolang.com/pkgdoc)
- [菜鸟教程：Go语言时间和日期](https://www.runoob.com/go/go-time.html)

# 参见

- 如何比较两个日期 - Getting the current date in Go
- 如何做日期加减运算 - Performing date arithmetic in Go