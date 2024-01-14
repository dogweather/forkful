---
title:    "Elm: 获取当前日期"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## 为什么要获取当前日期？

在编写程序时，经常需要获取当前的日期。无论是用于记录日志、生成报告还是其他用途，获取当前日期都是非常有用的。在 Elm 中，有几种方法可以获取当前日期，让我们来看看如何实现。

## 如何获取当前日期？

在 Elm 中，我们可以通过 `Time.now` 函数来获取当前日期。让我们来看看下面的代码示例：

```Elm
import Time

currentTime : Time.Posix
currentTime = Time.now

main : Html msg
main =
  text (toString currentTime)
```

上面的代码将输出类似于 `1562819723` 的时间戳，代表当前的日期和时间。我们也可以使用 `Time.millisToUtc` 函数来将这个时间戳转换为人类可读的日期。

```Elm
import Time

currentTime : Time.Posix
currentTime = Time.now

utcTime : Time.Posix
utcTime = Time.millisToUtc currentTime

main : Html msg
main =
  text (Time.toUtcString utcTime)
```

上面的代码将输出类似于 `2019-07-11T16:35:23Z` 的日期和时间字符串，这是 UTC 格式的时间。我们还可以使用 `Time.toLocalString` 来将时间戳转换为本地时间格式。

## 深入了解获取当前日期

在 Elm 中，`Time.now` 函数返回的是一个 `Time.Posix` 类型的值，它表示从 1970 年 1 月 1 日至今经过的秒数。我们可以使用 `Time.fromMillis` 函数来将时间戳转换为 `Time.Posix` 类型。

另外，我们也可以使用 `Time.toYearMonthDay` 函数来提取具体的年、月、日等信息。`Time` 模块还提供了许多其他方便的函数来处理日期和时间，建议在需要时仔细阅读文档。

## 参考链接

- Elm 官方文档：https://guide.elm-lang.org/
- Elm 中文社区：https://elm-china.org/
- `Time` 模块文档：https://package.elm-lang.org/packages/elm/time/latest/