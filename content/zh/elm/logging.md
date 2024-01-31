---
title:                "日志记录"
date:                  2024-01-26T01:03:48.560757-07:00
model:                 gpt-4-1106-preview
simple_title:         "日志记录"

category:             "Elm"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/logging.md"
---

{{< edit_this_page >}}

## 是什么 & 为什么?
日志记录本质上是指在软件运行时记录事件和数据输出的过程，想象它就像软件的日记。程序员使用日志记录来跟踪软件内部发生的事情——这对于调试问题、实时监控系统行为以及分析过去的活动以进行性能优化或审计都是极其宝贵的。

## 如何操作:
Elm的架构不支持开箱即用的副作用，比如日志记录——你通过命令来处理它们，这些命令是你的应用架构的一部分。为了教学目的，让我们来看一下如何通过端口向JavaScript发送消息来模拟日志记录。

首先，你需要定义一个端口模块：

```Elm
port module Logger exposing (..)

-- 定义一个端口将日志发送到JavaScript
port log : String -> Cmd msg
```

在你的 `Main.elm` 中，你会使用 `log` 端口来发送日志消息：

```Elm
import Logger exposing (log)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AnEvent ->
            -- 这里对模型进行一些更新
            ( updatedModel, log "发生了一个事件。" )

        AnotherEvent ->
            -- 这里进行其他模型更新
            ( anotherUpdatedModel, log "发生了另一个事件。" )
```

在JavaScript方面，你会订阅 `log` 端口来处理传入的日志消息：

```JavaScript
var app = Elm.Main.init({ /* ... */ });

app.ports.log.subscribe(function(message) {
    console.log(message);
});
```

然后在JavaScript控制台的样例输出将会是：

```
发生了一个事件。
发生了另一个事件。
```

## 深入探讨
传统上，在像Python或Java这样的语言里，通过使用日志库来进行日志记录，这些日志库提供了一个简洁的API来在不同级别记录消息，如debug、info、warning、error和critical。

Elm，因其专注于纯净性和不可变性，并不提供这种直接的日志记录，因为任何类型的IO或副作用都是通过Elm架构来明确管理的。

当你在Elm中需要全功能的日志记录时，你通常依赖于外部的JavaScript工具。如上所示的端口就是通向这些工具的桥梁。Debug模块是另一个选项，但它仅用于开发，不适用于生产日志记录。

除了端口之外，程序员经常利用Elm编译器信息和运行时调试设施，比如 `Debug.log`，你可以将它插入到你的代码中来追踪值。它会将一个表达式包装起来，并将其输出记录到控制台，如下所示：

```Elm
view model =
    Debug.log "模型调试" model
    -- 这里是你的视图代码
```

然而这也不适用于生产环境。像elm-logger这样的工具对端口进行了一些抽象处理以用于日志记录，尽管这些也更多地用于开发而不是生产。

## 另请参阅
- Elm端口: https://guide.elm-lang.org/interop/ports.html
- Elm `Debug`: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Elm的日志讨论: https://discourse.elm-lang.org/t/elm-and-logging/546
- JavaScript控制台API: https://developer.mozilla.org/en-US/docs/Web/API/Console
- elm-logger包: https://package.elm-lang.org/packages/arkgil/elm-logger/latest/
