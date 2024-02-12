---
title:                "读取命令行参数"
aliases:
- /zh/elm/reading-command-line-arguments.md
date:                  2024-01-20T17:55:52.814515-07:00
model:                 gpt-4-1106-preview
simple_title:         "读取命令行参数"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? 什么与为什么？
读取命令行参数就是获取启动程序时用户输入的信息。程序员这么做是为了让程序更灵活，能根据不同参数执行不同任务。

## How to: 如何操作
Elm目前不直接支持命令行参数，因为它主要面向前端编程。不过，你可以用Node.js来与Elm交互，获取命令行参数。以下是在Node环境中使用JavaScript和Elm协作的例子。

```Elm
-- 在Elm中，你会这样定义一个程序模型
module Main exposing (..)

import Platform

type alias Model = 
    { args : List String }

type Msg 
    = SetArgs (List String)

main : Program () Model Msg
main =
    Platform.worker
        { init = \_ -> ( { args = [] }, Cmd.none )
        , update = update
        , subscriptions = \_ -> Sub.none
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetArgs args ->
            ( { model | args = args }, Cmd.none )

-- 省略其他无关代码
```

```JavaScript
// 使用JavaScript启动Elm程序并传递命令行参数
const { Elm } = require('./Main.elm');

const app = Elm.Main.init({
  flags: process.argv.slice(2)
});

app.ports.outgoing.subscribe(args => {
  console.log(args);
});
```

运行你的JavaScript脚本时，它会打印传递给Elm程序的命令行参数。

## Deep Dive 深入探讨
历史上，命令行参数是在后台作业和脚本中交互的主要方式，让程序可以处理自定工作或响应不同的启动状态。虽然Elm缺乏原生命令行处理能力，但它可以通过与JavaScript的互操作性来弥补。JavaScript的`process.argv`是个数组，它包含了启动Node.js进程时的所有命令行参数。

此外，还有其他工具和语言如Haskell的Dhall库或Rust的clap库可以用来处理命令行参数，但这取决于你的项目需求和环境。

实现细节方面，Elm在使用`Platform.worker`时处理外部消息，你可以发送自定义消息通知Elm应用参数变化。JavaScript部分作为胶水，通过启动Elm应用并传递`process.argv`作为服务端或脚本中使用Elm的桥梁。

## See Also 相关链接
- [Elm官方指南](https://guide.elm-lang.org/)
- [Node.js官方文档](https://nodejs.org/api/process.html#process_process_argv)
