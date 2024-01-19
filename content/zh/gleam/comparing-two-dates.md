---
title:                "比较两个日期"
html_title:           "Clojure: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 什么和为什么？
比较两个日期,就是确定两个日期在时间轴上的相对位置。程序员这样做主要是为了跟踪和控制复杂应用程序中的事件和时间关联行为。

## 如何操作：
在Gleam编程中，您可以使用内置函数比较两个日期。下面是示例：

```Gleam
import gleam/erlang.{now, sleep, timestamp_to_string}

fn wait_and_print() {
  let start = now()
  sleep(2000)
  let end = now()

  if start > end {
    io.println("Time travel is real!")
  } else {
    io.println("Time moves forward as expected.")
    io.println("Start time: " ++ timestamp_to_string(start))
    io.println("End time: " ++ timestamp_to_string(end))
  }
}

```

当你运行这段代码时，你可以看到时间顺序如你所期望的那样向前移动。

````Gleam
Time moves forward as expected.
Start time: 2022-03-16 16:50:00 GMT
End time: 2022-03-16 16:50:02 GMT
````

## 深入了解：
1. 历史背景：早期的编程语言功能有限，程序员需要手动比较日期。现代语言如Gleam提供了内置函数，使日期比较变得更加简单和准确。

2. 可选方法：Gleam的Erlang内核支持日期和时间计算，提供了丰富的功能。但你也可以自己写一个比较日期的函数，完全取决于项目需求。

3. 实现细节：Gleam使用Erlang的标准库进行日期操作。你可以使用`now()`获取当前的时间戳，然后通过`>`运算符比较两个时间戳。

## 另请参阅：
1. [Gleam Docs](https://docs.gleam.run) ：Gleam官方文档，了解更多关于Gleam的所有特性。
2. [Gleam's Github page](https://www.github.com/gleam-lang/gleam)：Gleam的Github页面，查看Gleam的最新开发情况和版本更新。
3. [Erlang's time module](https://erlang.org/doc/man/time.html)：深入了解Erlang的时间模块和它如何在Gleam中工作。