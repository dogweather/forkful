---
title:                "Haskell: 计算未来或过去的日期"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Learn Haskell - 认识Haskell

为什么：计算未来或过去日期是一项有用的技能，它可以帮助我们有效地安排时间和计划未来的任务。

如何计算日期：在Haskell中，我们可以使用Data.Time模块来计算日期。下面是一个简单的示例，在当前日期的基础上添加两周并输出结果。

```Haskell
import Data.Time

main :: IO ()
main = do
    currentTime <- getCurrentTime
    let futureDate = addDays 14 (utctDay currentTime)
    print futureDate
```

输出结果：2021-07-13

深入了解：我们可以通过组合`addDays`、`addMonths`和`addYears`等函数来计算复杂的日期。此外，我们还可以使用`diffDays`和`diffMonths`函数来比较两个日期之间的差距。这些函数在处理日期的过程中非常有用，并有助于我们更好地了解时间。

另请参阅：

- [Haskell官方文档](https://www.haskell.org/documentation/)
- [Data.Time模块文档](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Haskell编程语言简介](https://www.geeksforgeeks.org/haskell-programming-language/#:~:text=Haskell%E7%94%B1%E4%B8%80%E4%B8%AA%E4%BB%A3%E7%A0%81%E7%AE%A1%E7%90%86%E5%99%A8%E5%89%8D%E7%BE%8E%E5%AD%A6Haskell%EB%B0%94%EB%A5%B4%E7%94%9F%E6%95%88%EF%BC%8C,%E5%8F%AF%E8%A7%A3%E5%86%B3%E7%9A%84%E5%92%8C%E5%93%88%E5%B8%8C%E5%B8%8C%E9%81%87%E5%88%B0%E7%9A%84%E9%97%AE%E9%A2%98%E3%80%82)