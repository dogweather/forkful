---
title:                "Elm: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

为什么：是什么驱使我们把日期转换为字符串？

日期和时间在编程中是非常常见的概念，我们经常需要在我们的程序中使用它们来记录和处理数据。但是，日期和时间的格式可能因国家和文化而异，因此需要进行转换为字符串来确保统一性和易读性。

## 如何进行日期转换为字符串

使用Elm编程语言，我们可以使用现成的函数来将日期转换成字符串。让我们来看一个简单的例子：

```Elm
import Time exposing (Date, fromString, toString)

-- 将日期从字符串转换为Date类型
date = fromString "2021-09-10"
-- 将Date类型转换为字符串
stringDate = toString date

-- 输出 "2021-09-10"
```

在这个例子中，我们使用`fromString`函数将字符串格式的日期转换为Date类型，并且使用`toString`函数将其转换回字符串。我们还可以在转换时指定所需的日期格式，例如：

```Elm
Date = fromString "2021-09-10"
-- 使用 "年-月-日" 格式来转换为字符串
formattedDate = toString "yyyy-MM-dd"

-- 输出 "2021-09-10"
```

这样，我们就可以根据需求来灵活地转换日期格式。

## 深入了解日期转换为字符串

在进行日期转换时，还需要考虑时区和语言的影响。Elm提供了`zone`和`language`参数来更准确地进行日期转换，以确保输出的字符串与所在区域和语言一致。

除了从字符串转换为Date类型，我们也可以使用`toLocalString`函数将Date类型转换为本地化的字符串。例如：

```Elm
Date = fromString "2021-09-10"
-- 将Date类型转换为本地化的字符串
localizedDate = toLocalString "yyyy-MM-dd" "zh-CN"

-- 输出 "2021年09月10日"
```

在这个例子中，我们指定了语言参数为“zh-CN”，因此输出的字符串为中文格式的日期，符合中国语言和文化习惯。

## 参考资料

- Elm语言官方文档：https://guide.elm-lang.org
- Time库文档：https://package.elm-lang.org/packages/elm/time/latest/