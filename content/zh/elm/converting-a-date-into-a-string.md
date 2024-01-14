---
title:    "Elm: 日期转换为字符串"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# 为什么

为什么要将日期转换为字符串呢？在Elm编程中，有时我们需要将日期转换为字符串，例如在显示用户生日或者活动日期时。使用Elm内置的函数可以轻松地完成这项任务，让我们来看看如何操作吧！

# 如何操作

在Elm中，我们可以使用 `>>=` 运算符来连接内置函数来完成日期到字符串的转换。首先，我们需要导入 `Date` 模块：

```
import Date
```

接下来，我们将创建一个 `Date` 对象用于转换。假设我们想要将今天的日期转换为字符串，我们可以这样写：

```
Date.fromTime  (Time.now)
```

这将返回一个 `Result` 类型的值，我们可以使用 `Date.toOrdinal` 函数来将其转换为日期的字符串表示形式。让我们来看一个完整的例子：

```
Date.fromTime  (Time.now) >>= Date.toOrdinal
```

输出结果将会是类似于 `2021-08-08T00:00:00.000Z` 的字符串。我们可以使用 `Date.customFormat` 函数来自定义日期的格式，例如只保留月份和日期：

```
Date.fromTime  (Time.now) >>= Date.customFormat "%m/%d"
```

最终输出将会是 `08/08` 。

# 深入了解

除了 `Date.toOrdinal` 和 `Date.customFormat` 函数，Elm还提供了其他用于日期转换的函数。例如，`Date.dayOfWeek` 函数可以返回日期是星期几，`Date.month` 函数可以返回日期的月份等等。您可以参考 [Elm官方文档](https://package.elm-lang.org/packages/elm/time/latest/) 来了解更多可用的函数。

# 参考链接

- [Elm官方文档](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm教程 - 日期时间](https://elmprogramming.com/date-time.html)
- [Elm日期转换示例](https://gist.github.com/dylanowen/1202230)