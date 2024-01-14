---
title:    "Elm: 将日期转换为字符串"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么
有时候，我们需要将日期转换为字符串表示，例如在日历应用程序或日期选择器中。这可以让我们更容易地显示日期，比如在显示生日或会议时间时。

## 如何转换日期为字符串
```elm
import Time
import Date.Format

-- 创建一个日期对象
date = Date.fromIsoString "2021-01-01"

-- 使用Date.Format来将日期转换为特定格式的字符串
stringDate = Date.Format.format "%Y年%m月%d日" date

-- 输出结果为："2021年01月01日"
``` 

在上面的例子中，我们首先导入 `Time` 和 `Date.Format` 模块，然后我们创建了一个日期对象。接着，我们使用 `Date.Format` 中的 `format` 函数将日期对象转换为我们想要的格式。最后，我们将结果存储在 `stringDate` 变量中，并可以使用它来在我们的程序中显示日期。

## 深入了解
要深入了解如何将日期转换为字符串，我们需要更多的了解 `Date` 和 `Time` 模块。 `Date` 模块提供了一些基本函数来创建、修改和比较日期对象。 `Time` 模块提供了一些函数来处理时间单位，比如秒、分和时。我们可以使用这些单位来构建我们想要的日期格式。

## 参考链接
- [Elm官方文档：Date模块](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Elm官方文档：Time模块](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Date模块源代码](https://github.com/elm-lang/core/blob/master/src/Date.elm)
- [Time模块源代码](https://github.com/elm-lang/core/blob/master/src/Time.elm)

## 相关链接