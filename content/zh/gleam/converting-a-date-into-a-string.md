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

## 什么是日期转换成字符串？

把日期转换成字符串是编程中常见的任务，它可以将一个日期数据转换成文字格式的字符串，以便更容易地理解和处理。程序员经常进行这样的转换是因为日期数据的存储格式通常不太直观，而转换成字符串后则可以更加直观地展示出来。

## 如何实现日期转换成字符串？

Gleam提供了一个内置函数 `format_date`，可以用来将日期数据转换成字符串。例如，我们想要将日期11月13日转换成字符串并显示出来，可以按照以下格式操作：

```Gleam
format_date(2021-11-13, "%Y年%m月%d日")
```

运行后，会得到 `2021年11月13日` 作为输出结果。

## 深入了解

历史上，日期转换成字符串是为了更方便的将日期展示给用户。然而，现在也可以通过使用其他技术解决这一问题，如JavaScript中的 `toLocaleDateString()` 函数。

在Gleam中，日期转换成字符串还可以根据不同的格式要求来进行自定义，具有一定的灵活性和可操作性。内置函数 `format_date` 中的 `%Y` `%m` `%d` 即为标识符，可以根据需要自由搭配使用来表现出不同的日期格式。

## 查看更多

了解更多关于Gleam日期转换成字符串的知识，请访问官方文档：https://gleam.run/documentation/standard_library/#datetime

同时，也可以参考其他有关日期转换的文章进行学习与实践。