---
title:                "获取当前日期"
date:                  2024-01-20T15:14:08.177241-07:00
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? 什么以及为什么？

获取当前日期是读取系统时钟以确定现在的日期和时间。程序员常常需要这功能来记录事件，计算间隔或展示实时数据。

## How to: 如何操作

在Elm中，你不能直接获取当前日期，因为Elm是纯函数式的。你需要用到`Time`模块。首先，初始化一个命令去请求当前的时间戳（epoch时间），然后将它转换成日期。

```Elm
import Browser
import Task
import Time exposing (Posix)

type Msg = NewTime Posix

init : () -> ( Model, Cmd Msg )
init () =
    ( Model, Task.perform NewTime Time.now )

-- 当Time.now的响应返回时，NewTime消息会被发送给update函数。
-- 你可以在Model里存储这个日期，或者进行其他操作。
```

假设你想要格式化这个日期：

```Elm
import Time

-- 传递Posix时间给format函数来得到字符串表示。
formatDate : Posix -> String
formatDate posix =
    Time.toIsoString posix
```

## Deep Dive 深度探讨

获取当前日期的函数在很多编程语言中都是基本的，但在Elm里稍微复杂。因为Elm的纯函数式特性，直接访问系统时间会引入副作用，这是Elm设计哲学中不被鼓励的。作为替代，Elm使用`Time`模块，并通过命令（Cmd）处理时间数据。

如果你不使用Elm的默认`Time`模块，有些第三方库提供了一些封装来操作日期和时间。比如说，`elm-time`库可以提供一些便利的日期处理函数。

在实现上，获取当前日期涉及到和JavaScript的交互，因为Elm代码最终会被编译为JavaScript。JavaScript的`Date`对象被用于后台来获取实时时间。Elm通过使用端口（ports）或者特殊的时间模块来安全地管理这个交互过程，遵循其它的纯函数式语言的理念。

## See Also 相关链接

- [Elm Time Documentation](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm Programming Guide](https://guide.elm-lang.org/)
- [Elm Ports](https://guide.elm-lang.org/interop/ports.html)
- [Unix Time Converters](https://www.unixtimestamp.com/)
