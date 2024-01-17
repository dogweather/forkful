---
title:                "获取当前日期"
html_title:           "Gleam: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 当前日期是什么，为什么程序员要获取它？

当前日期是指今天的日期，通常以月/日/年的格式表示。程序员通常需要获取当前日期来跟踪工作时间、生成文件名或日志，或者简单地用于程序中的时间戳。

## 如何获取当前日期：

要获取当前日期，我们可以使用Gleam语言中的内置函数`Time.now()`。让我们看看下面的示例代码：

```Gleam
import gleam/time

fn main() {
  let current_date = Time.now()
  IO.println(current_date)
}
```

运行以上代码将输出当前日期，例如：`2021-08-18`。

## 深入了解：

- 历史背景：在过去，程序员通常需要手动编写代码来获取当前日期。但随着计算机技术的发展，操作系统开始提供内置函数来获取日期，如今Gleam语言也提供了内置函数来简单地获取当前日期。
- 其他方法：除了Gleam语言提供的`Time.now()`函数，程序员们还可以使用其他语言和工具来获取当前日期。例如，在JavaScript中，可以使用`new Date()`函数来获取当前日期。
- 实现细节：`Time.now()`函数在Gleam语言中使用标准库中的`os.date()`函数来获取当前日期，并返回一个`Y-m-d`格式的字符串。

## 参考链接：

- Gleam语言官方网站：https://gleam.run/
- 文档中关于日期和时间的指南：https://gleam.run/articles/guides/datetime/
- JavaScript中获取当前日期的方法：https://www.w3schools.com/js/js_date_methods.asp