---
title:                "获取当前日期"
date:                  2024-01-20T15:16:34.340916-07:00
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在编程中获取当前日期是一项基本操作，用于记录事件时间、定时任务或者有效期检查。了解当前日期对于程序来说相当重要，以保证数据和事件能够及时且正确地处理。

## 如何操作：
```Ruby
require 'date'

# 获取今天的日期
today = Date.today
puts today
# 输出例子：2023-04-05

# 获取现在的日期和时间
now = DateTime.now
puts now
# 输出例子：2023-04-05T14:33:15+00:00

# 获取时间戳
time_stamp = Time.now
puts time_stamp
# 输出例子：2023-04-05 14:33:15 +0000
```

## 深入探讨
在 Ruby 中，`Date` 和 `Time` 类被广泛用于处理日期和时间。`Date` 类处理的是日期部分，而 `DateTime` 和 `Time` 类则能够处理更详细的时间信息，包括小时、分钟和秒。

如果只需要日期，`Date.today` 是最直接的方法。但如果你需要更精确的时间点，`DateTime.now` 或 `Time.now` 可以提供更多详细信息。Ruby 的时间处理功能在过去经过多次迭代和优化，其中，`Time` 类是基于操作系统的时间功能构建的，而 `Date` 和 `DateTime` 是纯 Ruby 实现，可能在性能上略逊一筹。

有时候，你可能需要不同格式的日期和时间，这时 `strftime` 方法就派上用场了。比如，`Time.now.strftime("%Y-%m-%d %H:%M:%S")` 可以以 "年-月-日 时:分:秒" 的格式输出当前时间。

## 相关链接
- Ruby 官方文档中的 Time 类: [https://ruby-doc.org/core/Time.html](https://ruby-doc.org/core/Time.html)
- strftime 方法格式选项: [https://apidock.com/ruby/DateTime/strftime](https://apidock.com/ruby/DateTime/strftime)
