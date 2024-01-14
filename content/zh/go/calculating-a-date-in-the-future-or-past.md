---
title:                "Go: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 为什么

人们可能想要计算未来或过去日期的原因可能是为了日程安排、生日、节假日等。使用Go编程语言可以轻松地进行日期计算。

## 如何

```Go
package main

import (
    "fmt"
    "time"
)

func main(){
    // 假设今天是2021年6月15日
    currentTime := time.Now()
    fmt.Println("当前日期：", currentTime)

    // 计算3天后的日期
    futureDate := currentTime.AddDate(0, 0, 3)
    fmt.Println("3天后的日期：", futureDate)

    // 计算1个月前的日期
    pastDate := currentTime.AddDate(0, -1, 0)
    fmt.Println("1个月前的日期：", pastDate)
}
```

输出结果：
```
当前日期： 2021-06-15 18:00:00 +0800 CST
3天后的日期： 2021-06-18 18:00:00 +0800 CST
1个月前的日期： 2021-05-15 18:00:00 +0800 CST
```

## 深入了解

在Go编程语言中，可以使用`time`包中的`AddDate()`函数来进行日期计算。该函数接受三个参数，分别为年、月、日的增加/减少值，返回结果为一个新的`time.Time`类型的日期。

需要注意的是，月份是从1开始计数，而在`AddDate()`函数中，月份的增加/减少值可以超过12或小于1，该函数会自动将日期调整为合理的日期。例如，如果当前日期为1月31日，增加1个月后的日期为2月31日，则该函数会将日期调整为2月28日/29日，避免出现无效的日期。

# 参考链接

- [Go语言官方文档-时间包](https://golang.org/pkg/time/)
- [Go语言中文网-日期时间操作](https://studygolang.com/articles/14386)
- [Go编程语言圣经-日期与时间](https://books.studygolang.com/gopl-zh/ch8/ch8-04.html)

# 参见

- [使用Go编程语言计算日期和时间](https://www.example.com/)
- [Go语言中日期时间格式化](https://www.example.com/)