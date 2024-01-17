---
title:                "获取当前日期"
html_title:           "Haskell: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 简介
Haskell是一个优秀的编程语言，在当前版本中，它提供了强大的功能来获取当前日期。对于程序员来说，获取当前日期是一个常见的需求，因为它可以帮助我们跟踪时间并将其与其他数据相关联。

# 什么及为什么
获取当前日期意味着获取当天的日期和时间。它是一个常见的需求，因为它可以帮助我们跟踪事件发生的时间，例如启动一个程序、保存数据等等。程序员通常会使用它来记录日志或调试错误。

# 如何
要在Haskell中获取当前日期，我们可以使用Data.Time库中的getCurrentTime函数。首先，我们需要导入Data.Time库：
```
import Data.Time
```
然后，我们可以使用getCurrentTime函数来获取当前日期：
```
currentDate <- getCurrentTime
```
最后，我们可以使用formatTime函数来格式化日期的显示方式，例如：
```
let formattedDate = formatTime defaultTimeLocale "%Y-%m-%d" currentDate
```
这将返回一个带有年份、月份和日期的字符串，例如“2021-04-20”。

# 深入探讨
历史背景：在早期版本的Haskell中，并没有提供内置的方法来获取当前日期。直到最新版本，Haskell才提供了Data.Time库来满足这一需求。

其他选择：除了使用getCurrentTime函数，我们也可以使用其他外部库来获取当前日期，例如Data.Time.Clock.System库和Data.Time.Clock.POSIX库。

实现细节：getCurrentTime函数会返回当前日期和时间的UTCTime类型。我们可以使用formatTime函数来格式化日期的显示方式。除了上述示例中提到的默认格式外，我们还可以使用自定义的格式来显示日期和时间的不同部分。

# 相关链接
- [Haskell官方网站](https://www.haskell.org/)
- [Data.Time文档](https://downloads.haskell.org/)