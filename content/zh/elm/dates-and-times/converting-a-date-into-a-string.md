---
date: 2024-01-20 17:36:46.839232-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C) \u5728Elm\u4E2D\uFF0C\u65E5\u671F\
  \u548C\u65F6\u95F4\u662F\u901A\u8FC7`Time.Posix`\u7C7B\u578B\u6765\u7BA1\u7406\u7684\
  \uFF0C\u8FD9\u662FUnix\u65F6\u95F4\u6233\u7684\u5C01\u88C5\u3002`Date`\u6A21\u5757\
  \u7528\u4E8E\u5C06Posix\u65F6\u95F4\u6233\u8F6C\u6362\u6210\u65E5\u671F\u5BF9\u8C61\
  \u3002\u800C`Date.Format`\u5219\u63D0\u4F9B\u683C\u5F0F\u5316\u7684\u51FD\u6570\uFF0C\
  \u5141\u8BB8\u6211\u4EEC\u628A\u65E5\u671F\u5BF9\u8C61\u8F6C\u5316\u6210\u7279\u5B9A\
  \u7684\u5B57\u7B26\u4E32\u683C\u5F0F\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:46.845937-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C) \u5728Elm\u4E2D\uFF0C\u65E5\u671F\u548C\u65F6\
  \u95F4\u662F\u901A\u8FC7`Time.Posix`\u7C7B\u578B\u6765\u7BA1\u7406\u7684\uFF0C\u8FD9\
  \u662FUnix\u65F6\u95F4\u6233\u7684\u5C01\u88C5\u3002`Date`\u6A21\u5757\u7528\u4E8E\
  \u5C06Posix\u65F6\u95F4\u6233\u8F6C\u6362\u6210\u65E5\u671F\u5BF9\u8C61\u3002\u800C\
  `Date.Format`\u5219\u63D0\u4F9B\u683C\u5F0F\u5316\u7684\u51FD\u6570\uFF0C\u5141\u8BB8\
  \u6211\u4EEC\u628A\u65E5\u671F\u5BF9\u8C61\u8F6C\u5316\u6210\u7279\u5B9A\u7684\u5B57\
  \u7B26\u4E32\u683C\u5F0F\u3002"
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
weight: 28
---

## How to: (如何操作)
```Elm
import Time exposing (Posix)
import Date exposing (fromPosix)
import Date.Format exposing (format)

-- 假设你有一个Posix时间戳
timeStamp : Posix
timeStamp = Time.millisToPosix 1617973119077

-- 转换成人类可读格式
readableDate : String
readableDate = time |> fromPosix |> format "yyyy-MM-dd HH:mm:ss"

-- 输出: "2021-04-09 11:45:19"
```

## Deep Dive (深入探索)
在Elm中，日期和时间是通过`Time.Posix`类型来管理的，这是Unix时间戳的封装。`Date`模块用于将Posix时间戳转换成日期对象。而`Date.Format`则提供格式化的函数，允许我们把日期对象转化成特定的字符串格式。

不同编程语言有自己处理日期和时间的方法。在Elm之前，JavaScript等语言通过类似的方式实现日期到字符串的转换。Elm提供的`Date`库简洁且功能强大。

对于转换和格式化，除了`Date.Format`，可以使用第三方库如`elm-community/elm-time`增加更多功能。

## See Also (另请参阅)
- Elm Date documentation: [https://package.elm-lang.org/packages/elm/time/latest/](https://package.elm-lang.org/packages/elm/time/latest/)
- Date.Format documentation: [https://package.elm-lang.org/packages/ryannhg/date-format/latest/](https://package.elm-lang.org/packages/ryannhg/date-format/latest/)
- elm-community/elm-time: [https://package.elm-lang.org/packages/elm-community/elm-time/latest/](https://package.elm-lang.org/packages/elm-community/elm-time/latest/)
