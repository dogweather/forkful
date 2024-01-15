---
title:                "将日期转换为字符串"
html_title:           "Gleam: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么
日期是编程中经常遇到的数据类型，将其转换成字符串可以方便输出和处理。在Gleam中，日期和字符串之间的转换是一个基本的操作，下面我们将介绍如何实现。

## 如何实现
```Gleam
import Gleam.Time

let date = Time.OffsetDate(year: 2021, month: 8, day: 15, offset_minutes: 0)

// 将日期转换为字符串
//// 使用默认格式
let date_str = Time.format(date) // "2021-08-15T00:00:00+00:00"
//// 自定义格式
let custom_format = "%Y/%m/%d"
let date_str = Time.format(date, custom_format) // "2021/08/15"

// 获取当前日期和时间
let now = Time.localNow()
let now_str = Time.format(now) // "2021-08-16T13:45:06+08:00"

// 解析字符串为日期
let date_str = "2021-08-15T00:00:00+00:00"
let date = Time.fromString(date_str)
```

运行以上代码，我们可以看到转换后的字符串和日期的格式是一致的。Gleam提供了多种内置的格式选项，也可以根据自己的需要定义格式。另外，Gleam还提供了便捷的获取当前日期和解析字符串为日期的方法。

## 深入了解
在Gleam中，日期的表示使用了OffsetDateTime结构体，它包含了日期、时间和时区的信息。在转换为字符串时，Gleam会根据时区自动进行时区转换，无需手动指定。Gleam也支持按照ISO 8601标准格式化日期，可以在定义格式时指定。

## 参考链接
- [Gleam's Date and Time module](https://gleam.run/core-libraries/date-time.html) (官方文档)
- [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) (标准格式介绍)
- [Gleam社区论坛](https://gleam.run/community.html) (官方论坛，可以在这里提出问题和交流)