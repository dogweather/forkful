---
date: 2024-01-26 01:03:48.560757-07:00
description: "\u5982\u4F55\u64CD\u4F5C: Elm\u7684\u67B6\u6784\u4E0D\u652F\u6301\u5F00\
  \u7BB1\u5373\u7528\u7684\u526F\u4F5C\u7528\uFF0C\u6BD4\u5982\u65E5\u5FD7\u8BB0\u5F55\
  \u2014\u2014\u4F60\u901A\u8FC7\u547D\u4EE4\u6765\u5904\u7406\u5B83\u4EEC\uFF0C\u8FD9\
  \u4E9B\u547D\u4EE4\u662F\u4F60\u7684\u5E94\u7528\u67B6\u6784\u7684\u4E00\u90E8\u5206\
  \u3002\u4E3A\u4E86\u6559\u5B66\u76EE\u7684\uFF0C\u8BA9\u6211\u4EEC\u6765\u770B\u4E00\
  \u4E0B\u5982\u4F55\u901A\u8FC7\u7AEF\u53E3\u5411JavaScript\u53D1\u9001\u6D88\u606F\
  \u6765\u6A21\u62DF\u65E5\u5FD7\u8BB0\u5F55\u3002 \u9996\u5148\uFF0C\u4F60\u9700\u8981\
  \u5B9A\u4E49\u4E00\u4E2A\u7AEF\u53E3\u6A21\u5757\uFF1A."
lastmod: '2024-04-05T22:38:46.839894-06:00'
model: gpt-4-1106-preview
summary: "Elm\u7684\u67B6\u6784\u4E0D\u652F\u6301\u5F00\u7BB1\u5373\u7528\u7684\u526F\
  \u4F5C\u7528\uFF0C\u6BD4\u5982\u65E5\u5FD7\u8BB0\u5F55\u2014\u2014\u4F60\u901A\u8FC7\
  \u547D\u4EE4\u6765\u5904\u7406\u5B83\u4EEC\uFF0C\u8FD9\u4E9B\u547D\u4EE4\u662F\u4F60\
  \u7684\u5E94\u7528\u67B6\u6784\u7684\u4E00\u90E8\u5206\u3002\u4E3A\u4E86\u6559\u5B66\
  \u76EE\u7684\uFF0C\u8BA9\u6211\u4EEC\u6765\u770B\u4E00\u4E0B\u5982\u4F55\u901A\u8FC7\
  \u7AEF\u53E3\u5411JavaScript\u53D1\u9001\u6D88\u606F\u6765\u6A21\u62DF\u65E5\u5FD7\
  \u8BB0\u5F55\u3002"
title: "\u65E5\u5FD7\u8BB0\u5F55"
weight: 17
---

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
