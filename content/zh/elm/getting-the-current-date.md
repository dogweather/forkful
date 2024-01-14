---
title:                "Elm: 获取当前日期"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

在编写任何类型的程序时，获取当前日期是一个非常常见的需求。无论是日程安排、定时任务还是文件命名，都需要使用到当前日期。因此，学习如何在Elm中获取当前日期是非常必要的。

## 如何操作

在Elm中，我们可以使用内置的`Time`包来获取当前日期。下面是一个简单的例子，展示如何获得当前日期并将其打印输出：

```Elm
import Time exposing (now)
import Date exposing (fromTime, Day)

main =
    now
        |> fromTime
        |> Day.format { year = Day.number, month = Day.twoDigits, day = Day.twoDigits }
        |> Debug.log "当前日期为："
```

运行以上代码，将会得到类似于`当前日期为：2021-07-07`的输出结果。首先，我们导入了`Time`和`Date`包，然后使用`now`函数来获取当前时间，再通过`fromTime`函数将时间转换为日期对象。最后，使用`Day.format`函数来将日期对象格式化为我们想要的形式，并将输出结果打印到控制台。

## 深入了解

除了上述的方法外，我们还可以使用更多的函数来获取更加精确的时间信息。比如，`Time.nowInUtc`函数可以返回当前时间的UTC格式，`Time.inSeconds`函数可以将时间转换为秒数，`Time.every`函数可以设置定时任务，等等。想要深入了解可以查阅[官方文档](https://package.elm-lang.org/packages/elm/time/latest/)。

## 参考资料

- [Elm官方文档](https://package.elm-lang.org/packages/elm/time/latest/)
- [使用 Elm 进行 Web 编程](https://elmprogramming.com/)
- [Learn Elm in Y minutes](https://learnxinyminutes.com/docs/zh-cn/elm-zh/)