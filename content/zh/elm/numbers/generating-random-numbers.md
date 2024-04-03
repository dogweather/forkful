---
changelog:
- 2024-02-27, dogweather, edited and tested
- 2024-02-27, gpt-4-0125-preview, translated from English
date: 2024-02-27 22:50:27.039281-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Elm\u7684\u7EAF\u51FD\u6570\u6027\u8D28\
  \u610F\u5473\u7740\u4F60\u4E0D\u80FD\u50CF\u5728\u547D\u4EE4\u5F0F\u8BED\u8A00\u4E2D\
  \u90A3\u6837\u76F4\u63A5\u751F\u6210\u968F\u673A\u6570\u3002\u76F8\u53CD\uFF0C\u60A8\
  \u9700\u8981\u7ED3\u5408\u4F7F\u7528`Random`\u6A21\u5757\u548C\u547D\u4EE4\u3002\
  \u8FD9\u662F\u4E00\u4E2A\u751F\u6210\u4ECB\u4E8E1\u5230100\u4E4B\u95F4\u7684\u968F\
  \u673A\u6574\u6570\u7684\u57FA\u672C\u793A\u4F8B\u3002 \u9996\u5148\uFF0C\u4F7F\u7528\
  \ `elm install elm/random` \u5B89\u88C5 `Random`\u2026"
lastmod: '2024-03-13T22:44:47.668195-06:00'
model: gpt-4-0125-preview
summary: "Elm\u7684\u7EAF\u51FD\u6570\u6027\u8D28\u610F\u5473\u7740\u4F60\u4E0D\u80FD\
  \u50CF\u5728\u547D\u4EE4\u5F0F\u8BED\u8A00\u4E2D\u90A3\u6837\u76F4\u63A5\u751F\u6210\
  \u968F\u673A\u6570\u3002\u76F8\u53CD\uFF0C\u60A8\u9700\u8981\u7ED3\u5408\u4F7F\u7528\
  `Random`\u6A21\u5757\u548C\u547D\u4EE4\u3002\u8FD9\u662F\u4E00\u4E2A\u751F\u6210\
  \u4ECB\u4E8E1\u5230100\u4E4B\u95F4\u7684\u968F\u673A\u6574\u6570\u7684\u57FA\u672C\
  \u793A\u4F8B."
title: "\u751F\u6210\u968F\u673A\u6570"
weight: 12
---

## 如何操作：
Elm的纯函数性质意味着你不能像在命令式语言中那样直接生成随机数。相反，您需要结合使用`Random`模块和命令。这是一个生成介于1到100之间的随机整数的基本示例。

首先，使用 `elm install elm/random` 安装 `Random` 模块。然后将其导入到您的Elm文件中，连同必要的HTML和事件模块，如下所示：

`src/Main.elm`

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, text, div)
import Html.Events exposing (onClick)
import Random
```

为了使这个例子自成一体，你可以添加这个样板代码：
```elm
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

init : () -> (Model, Cmd Msg)
init _ =
  (Model 0, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
```

接下来，定义一个**命令**来生成随机数。这包括设置一个`Msg`类型来处理一旦生成的随机数，一个`Model`来存储它，以及一个更新函数将它们全部关联起来。
```elm
type Msg
    = Generate
    | NewRandom Int

type alias Model = { randomNumber : Int }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            ( model, Random.generate NewRandom (Random.int 1 100) )

        NewRandom number ->
            ( { model | randomNumber = number }, Cmd.none )
```

要触发生成一个数字，您可以通过视图中的一个按钮发送一个 `Generate` 消息：
```elm
view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Random Number: " ++ String.fromInt model.randomNumber) ]
        , button [ onClick Generate ] [ text "Generate" ]
        ]
```

当你点击"Generate"按钮时，将显示1到100之间的一个随机数。

这种简单的方法可以被调整和扩展，利用`Random`模块中的其他函数产生随机浮点数、列表，甚至基于自定义类型的复杂数据结构，为你的Elm应用程序添加不可预测性提供了广阔的空间。

Elm指南提供了更多的细节。它还有[掷六面骰子的例子](https://guide.elm-lang.org/effects/random)。
