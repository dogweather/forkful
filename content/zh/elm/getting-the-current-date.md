---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
获取当前日期是一种获取计算机系统当前日期的编程行为。程序员这么做是为了记录事件、追踪时间或者对包含日期的数据进行操作。

## 如何做：
在Elm中，你可以使用`Time`模块中的`now`函数来获取当前日期和时间，代码如下：

```Elm
import Time exposing (..)

type Msg = NewTime Time.Posix

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTime newTime ->
            ( newTime, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 NewTime
```
注意，获取时间是一个命令操作，需要在程序更新循环的`update`部分进行处理。在这个例子中，我们每秒都会获取一次新的时间，因此我们看到的随着时间不断变化。

## 深入解析
在比Elm更早的编程语言（如：C或Java）中，获取当前日期和时间的方法有多种。然而，Elm的设计者为了简化编程流程和减少副作用，选择了统一使用基于命令的异步操作来处理所有I/O任务（包括获取时间）。

替代方案包括使用JavaScript通过端口来获取时间，然后在Elm中处理。但这会让代码的复杂度增加，并降低了Elm代码的可读性和可靠性。

在实现细节方面，Elm的`Time.Posix`是以Unix时间为基础进行的时间封装，能提供更具可读性和便携性的时间操作。

## 参考链接
1. Elm官方文档：[Elm Time](https://package.elm-lang.org/packages/elm/time/latest/)
2. Elm官方裸板：[Random.Generator](https://package.elm-lang.org/packages/elm/random/latest/Random-Generator)