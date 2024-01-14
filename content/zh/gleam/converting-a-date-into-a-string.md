---
title:                "Gleam: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

为什么：日期转换为字符串是编写程序时常用的操作，可以将日期数据转换成用户更易读的形式，方便程序的使用。

如何：在Gleam编程语言中，可以使用标准库中的Date模块来进行日期和字符串的转换。下面是一个简单的例子：

```Gleam
import Date

let date = Date.from_iso("2021-01-22")
let string = Date.format(date, "%Y年%m月%d日")
``` 

输出结果为：2021年01月22日

深入了解：除了上面的方法，Gleam还提供了其他的日期转换函数，可以满足不同的需求。比如，可以使用`Date.to_rfc2822`函数将日期转换为RFC 2822格式的字符串，也可以使用`Date.to_unix`函数将日期转换为Unix时间戳。同时，还可以根据具体情况选择不同的日期格式字符串来满足不同的输出需求。

另外，Gleam还提供了一些有用的工具函数来处理日期数据，比如可以使用`Date.add_days`来增加或减少一定的天数，也可以使用`Date.diff_in_days`来计算两个日期之间的天数差。这些函数都可以很方便地应用在实际的开发中。

探究更多关于日期转换为字符串的细节，可以参考Gleam的官方文档：https://gleam.run/documentation/standard_lib.html#Date 

另请参阅：

- Gleam官方文档：https://gleam.run/
- 在线Gleam代码运行平台：https://play.gleam.run/
- Gleam社区论坛：https://elixirforum.com/c/gleam-lang