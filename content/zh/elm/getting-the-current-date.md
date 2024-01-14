---
title:                "Elm: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么要获取当前日期

在编程中，经常需要获取当前的日期和时间。这可以用于生成日志、跟踪事件、或者简单地显示给用户，以便他们知道当前的日期和时间。在 Elm 中，获取当前日期可以通过使用内置的 `Time` 模块来实现。

# 如何实现获取当前日期

为了获取当前的日期，我们首先需要导入 `Time` 模块。然后，我们可以使用 `Time.now` 函数来获取当前的日期和时间。下面是一个简单的示例代码：

```elm
import Time exposing (now)

date = now
```

`date` 变量现在将包含一个被称为 `Time.Posix` 的数据结构，它包含当前日期和时间的所有信息。所以，如果我们想要只获取日期部分，我们可以使用 `Time.Posix.toDate` 函数。下面是一个完整的代码示例，展示如何获取当前日期的字符串格式：

```elm
import Time exposing (now)
import Time exposing (Posix, toDate)
import Time.Format exposing (format)

date = now
        |> Posix.toDate
        |> format "%Y-%m-%d"
```

最终，`date` 变量将包含一个格式为 `YYYY-MM-DD` 的字符串，表示当前日期。如果我们想要获取当前时间，我们可以使用 `Time.Format` 模块来格式化 `Time.Posix` 数据结构中的时间部分。

# 深入了解获取当前日期

在 Elm 中，日期和时间是不可变的数据类型。这意味着我们无法修改它们，只能通过操作函数来获取不同形式的日期和时间。在 `Time` 模块中，还有其他一些函数可以用来获取时区和夏令时信息，以及计算日期和时间之间的差值。

# 参考链接

- Elm 官方文档：https://elm-lang.org/docs
- `Time` 模块文档：https://package.elm-lang.org/packages/elm/time/latest/
- Elm 社区论坛：https://discourse.elm-lang.org/
- Elm 中文社区：https://elm-china.org/