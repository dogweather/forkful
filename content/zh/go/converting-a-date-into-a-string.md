---
title:                "Go: 将日期转换为字符串"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 为什么要将日期转换为字符串？

在Go语言中，日期和时间是常用的数据类型，我们经常需要将日期转换为字符串来方便地输出或存储。日期和字符串之间的转换也是一个很容易出错的地方，因此掌握这个操作是很重要的。

## 怎么做

首先，我们需要导入`time`包来处理日期和时间。

```Go
import "time"
```

接着，我们可以使用`time`包里的`Format()`函数来将日期转换为字符串。该函数接受两个参数，第一个参数是我们要转换的日期，第二个参数是一个格式化字符串，用来定义日期的输出格式。

```Go
time := time.Now() // 获取当前日期和时间
dateString := time.Format("Jan 02, 2006") // 将日期转换为字符串，格式为"月份 日，年份"
fmt.Println(dateString) // 输出结果：Mar 15, 2021
```

我们还可以使用`Parse()`函数来将字符串转换为日期。它接受两个参数，第一个参数是我们要转换的字符串，第二个参数是一个格式化字符串，用来定义字符串的解析格式。

```Go
dateString := "2021-03-15"
parsedDate, _ := time.Parse("2006-01-02", dateString) // 将字符串转换为日期，格式为"年份-月份-日"
fmt.Println(parsedDate) // 输出结果：2021-03-15 00:00:00 +0000 UTC
```

## 深入了解

在将日期转换为字符串时，我们需要注意格式化字符串的写法。其中，月份和日是用两位数表示的，例如`Jan`表示1月，`02`表示02日。年份使用4位数表示，并且必须为2006，这是由于Go语言的诞生日期为2006年1月2日。这个规则可以帮助我们更方便地记忆格式化字符串的写法。

此外，我们还可以在格式化字符串中使用其他的日期和时间信息，例如小时、分钟、秒、星期几等。具体的写法可以参考Go语言官方文档。

# 参考资料

- [Go语言官方文档 - 时间和日期格式化](https://golang.org/pkg/time/#pkg-constants)