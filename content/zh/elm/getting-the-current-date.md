---
title:    "Elm: 获取当前日期"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

获取当前日期是编程中一个常见的需求，它可以帮助我们记录事件、创建日历或者进行数据分析。在这篇文章中，我们将学习如何使用Elm编程语言来获取当前日期。

## 如何做

获取当前日期在Elm中非常简单。我们只需要使用Elm的内置Date模块中的now函数，它将返回一个表示当前日期的Date数据类型。以下是一个简单的例子：

```Elm
import Date exposing (now)

date = now
```

运行以上代码，你将得到类似于下面这样的输出：

```Elm
Js.Date.fromTime 159870190536
```

这个结果是一个Js.Date类型的值，它包含了当前日期和时间的信息。

## 深入探讨

让我们来深入了解一下获取当前日期的原理。首先，我们需要知道的是，Js.Date类型是一个表示Unix时间戳的值。Unix时间戳是从1970年1月1日起经过的秒数，它被广泛用于记录时间和日期。

在Date模块中，now函数会调用Date.fromTime函数来获取当前时间并将其转换为Unix时间戳。因此，我们可以通过手动调用fromTime函数来实现相同的效果。以下是一个示例代码：

```Elm
import Date exposing (fromTime)

now = fromTime (Time.millisToPosix 0)
```

这里使用了一个名为Time的内置模块，它提供了一些有用的函数来处理时间和日期相关的操作。我们将Unix时间戳0转换为Posix时间，然后传递给fromTime函数。Posix时间是指从1970年1月1日起经过的毫秒数。

## 参考链接

- Elm官方文档：https://elm-lang.org/docs
- Elm中的时间和日期：https://elmprogramming.com/time-and-date.html
- 时间和日期的Unix时间戳：https://www.unixtimestamp.com/