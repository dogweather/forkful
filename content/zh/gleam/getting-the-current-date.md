---
title:                "获取当前日期。"
html_title:           "Gleam: 获取当前日期。"
simple_title:         "获取当前日期。"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

人们经常需要获取当前日期，无论是为了管理日程安排、记录文件的创建日期还是简单地查看今天是几号，获取当前日期都是一个很常见的需求。

## 如何实现

在Gleam中获取当前日期十分简单，只需使用标准库中的`Time`模块即可。下面是一个示例代码，展示如何使用`Time.now()`函数来获取当前日期，并将结果打印出来：

```Gleam
use time

let current_date = Time.now()

io.print("今天是：" ++ Time.format(YYYY-MM-DD, current_date))
```

执行以上代码，你将会在终端上看到类似于`今天是：2021-06-08`的输出。

## 深入了解

`Time`模块除了提供获取当前日期的函数之外，还可以让你指定时区、格式化日期字符串，并且支持多种日期操作。如果你想进一步了解如何使用`Time`模块来处理日期，可以查阅[Gleam官方文档](https://gleam.run/documentation/)中关于Time模块的介绍。

## 查看更多

- [了解更多关于Gleam的基础知识](https://gleam.run/documentation/)
- [学习Gleam标准库中Time模块的使用](https://gleam.run/documentation/standard-library/time.html)
- [探索Gleam社区中其他开发者分享的有用资源](https://github.com/gleam-lang/awesome-gleam)