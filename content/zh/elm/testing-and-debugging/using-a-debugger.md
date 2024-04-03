---
date: 2024-01-26 03:50:11.118340-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Elm \u6CA1\u6709\u5185\u7F6E\u7684\u8C03\
  \u8BD5\u5668\uFF0C\u5C31\u50CF JavaScript \u90A3\u6837\u62E5\u6709\u6D4F\u89C8\u5668\
  \u5F00\u53D1\u5DE5\u5177\u3002\u7136\u800C\uFF0CElm \u793E\u533A\u5DF2\u7ECF\u6784\
  \u5EFA\u4E86\u4E00\u4E9B\u5DE5\u5177\u6765\u586B\u8865\u8FD9\u4E2A\u7F3A\u53E3\u3002\
  \u4E0B\u9762\u662F\u5982\u4F55\u4F7F\u7528 `elm-debug-transformer` \u6765\u8C03\u8BD5\
  \u4F60\u7684 Elm \u5E94\u7528\u7A0B\u5E8F\uFF1A."
lastmod: '2024-03-13T22:44:47.677862-06:00'
model: gpt-4-0125-preview
summary: "Elm \u6CA1\u6709\u5185\u7F6E\u7684\u8C03\u8BD5\u5668\uFF0C\u5C31\u50CF JavaScript\
  \ \u90A3\u6837\u62E5\u6709\u6D4F\u89C8\u5668\u5F00\u53D1\u5DE5\u5177\u3002\u7136\
  \u800C\uFF0CElm \u793E\u533A\u5DF2\u7ECF\u6784\u5EFA\u4E86\u4E00\u4E9B\u5DE5\u5177\
  \u6765\u586B\u8865\u8FD9\u4E2A\u7F3A\u53E3\u3002\u4E0B\u9762\u662F\u5982\u4F55\u4F7F\
  \u7528 `elm-debug-transformer` \u6765\u8C03\u8BD5\u4F60\u7684 Elm \u5E94\u7528\u7A0B\
  \u5E8F\uFF1A."
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
weight: 35
---

## 如何操作：
Elm 没有内置的调试器，就像 JavaScript 那样拥有浏览器开发工具。然而，Elm 社区已经构建了一些工具来填补这个缺口。下面是如何使用 `elm-debug-transformer` 来调试你的 Elm 应用程序：

```Elm
-- 安装 elm-debug-transformer （Node 包）

1. npm install -g elm-debug-transformer

-- 使用 elm-debug-transformer 启动你的应用

2. elm-debug-transformer --port=8000 yourMainElmFile.elm 
```

一旦 `elm-debug-transformer` 开始运行，它会创建一个用于记录的 WebSocket 连接。你将能在浏览器的控制台中看到调试信息，这里你可以检查你的程序在应用中某些点的数据结构。

在 Elm 0.19 及以后，`Debug` 模块的函数，如 `Debug.log` 和 `Debug.todo`，可以帮助你追踪值并有意标记代码中未完成的部分。下面是如何使用 Debug.log：

```Elm
import Debug

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( Debug.log "Incrementing" { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( Debug.log "Decrementing" { model | count = model.count - 1 }, Cmd.none )
```

你将在浏览器的控制台中看到 "Incrementing" 或 "Decrementing" 消息，连同 `model` 的新状态。

## 深入了解
Elm 的创作者 Evan Czaplicki 的目标是制作一种常见错误要么不可能发生要么容易捕捉的语言。这一哲学理念是为什么 Elm 的核心不包括传统的调试函数。Elm 的静态分析和类型推断在大幅减少运行时错误方面贡献巨大，这减少了对复杂运行时调试的需求。历史上的替代方案包括使用现已被废弃的 `elm-reactor`，它提供了时间旅行调试——一种在你的应用中回放和重放动作的方式。

今天，像 `elm-debug-transformer` 这样的工具和使用 Elm 的 `Debug` 模块帮助缩小了这一差距。虽然 `Debug` 模块只在开发期间使用，并且在构建生产版本之前应该被移除，但它是一个宝贵的工具，用于确定和记录状态改变。

请记住，传统的 JavaScript 调试技术，如断点或逐步执行，并不直接适用于 Elm，因为其架构和 Elm 运行时处理状态更新的方式。Elm 鼓励你构建程序，使得数据流清晰并且遵循严格的类型和不变性保证，最小化了调试的需求。

## 参见
- Elm 官方关于处理运行时异常的指南：https://guide.elm-lang.org/error_handling/
- `elm-debug-transformer` GitHub 仓库：https://github.com/kraklin/elm-debug-transformer
- 讨论调试策略的 Elm 话题讨论串：https://discourse.elm-lang.org/c/show-and-tell/debugging
- Elm 的 `Debug` 模块文档：https://package.elm-lang.org/packages/elm/core/latest/Debug
