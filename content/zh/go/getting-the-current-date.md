---
title:    "Go: 获取当前日期"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么

在编写程序时，经常需要获取当前的日期信息。无论是为了记录日志、计算时间间隔或者其他需求，获取当前日期的能力都是必不可少的。

# 如何

获取当前日期可以通过Go语言中的time包来实现。首先，我们需要导入time包：

```
import "time"
```

然后，可以使用time.Now()函数来获取当前的时间：

```
currentTime := time.Now()
```

接下来，我们可以使用time包提供的方法来提取所需的日期信息，比如年、月、日等：

```
year := currentTime.Year()
month := currentTime.Month()
day := currentTime.Day()
```

最后，我们可以将提取到的日期信息格式化输出，比如：

```
fmt.Printf("今天是%d年%d月%d日", year, month, day)
```

运行以上代码，你将会得到类似于以下输出：

```
今天是2021年7月1日
```

# 深入了解

除了上述介绍的常见方式，我们还可以通过time包提供的其他方法来获取更加精确的日期信息。比如，我们可以使用time.Now().Unix()函数来获取当前时间的时间戳，以秒为单位。或者，我们可以使用time.Now().UnixNano()函数来获取纳秒级的时间戳。

此外，time包还提供了一些方便的方法，比如Add()、Sub()和Truncate()等，来实现日期的加减和截断操作。如果你想进一步了解关于time包的知识，建议阅读官方文档或其他相关资料。

# 参考文献

- [Go官方文档 - Time](https://golang.org/pkg/time/)
- [Go官方文档 - TimeLayout](https://pkg.go.dev/time#TimeLayout)
- [Go语言圣经 - 第十三章 时间](https://books.studygolang.com/gopl-zh/ch13/ch13-05.html)

# 参见

- [理解 Go 语言中的 Time 包](https://mp.weixin.qq.com/s/itojsEwFJye3vZbNJu5IAg)
- [Go 语言官方博客 - Working with Time in Go](https://blog.golang.org/go-time)
- [Go 语言电子书 - Go 语言中的时间和日期处理](https://golang.design/under-the-hood/zh-cn/part2time/02time.html)