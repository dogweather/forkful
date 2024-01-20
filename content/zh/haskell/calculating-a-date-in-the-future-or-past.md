---
title:                "计算未来或过去的日期"
html_title:           "Haskell: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何为何？ (What & Why?)
将来或过去的日期计算是通过某一日期加减天数以获取新日期。程序员进行此操作以精确控制和管理时间。

## 如何操作: (How to)
在哈斯克尔（Haskell）中，我们可以使用 `addDays` 和 `diffDays` 函数来进行日期计算。让我们看一下这些例子：

```haskell
import Data.Time

-- 计算未来的日期
futureDate :: Day -> Integer -> Day
futureDate current days = addDays days current

-- 计算过去的日期
pastDate :: Day -> Integer -> Day
pastDate current days = addDays (-days) current

-- 例子
main :: IO()
main = do
    let today = fromGregorian 2022 1 1 -- 设置当前日期
    print $ futureDate today 365        -- 计算一年后的日期
    print $ pastDate today 365          -- 计算一年前的日期
```

## 深入了解 (Deep Dive)
日期运算的历史可以追溯到计算机编程的开始。在Haskell中，实现日期计算需要使用 `Data.Time` 库，它提供了跨越日期和时间的全面函数。

另一种替代方案是使用 `Day` 类型的 `diffDays` 方法计算两个日期之间的差异，然后通过加减法获取新日期。然而，在必要的情况下我们仍然可以手动来执行它。

一些实现细节要注意的是，`addDays` 函数会正常处理闰年和其他的日历规则，一切都隐藏在幕后，使得代码更简洁和安全。

## 另请参阅 (See Also)
为了更深入理解Haskell中的日期和时间，以下链接可能会有帮助：
1. [Haskell日期和时间教程](http://learnyouahaskell.com/dates-and-times)
2. [关于Haskell在Hackage的Data.Time库文档](https://hackage.haskell.org/package/time-1.11.1.1/docs/Data-Time.html)