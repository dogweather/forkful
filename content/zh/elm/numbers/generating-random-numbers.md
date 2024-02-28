---
title:                "生成随机数"
date:                  2024-02-27T22:50:27.039281-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-02-27, dogweather, edited and tested
  - 2024-02-27, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？
在Elm中生成随机数涉及使用`Random`模块来产生伪随机数，这在各种任务中都很方便，例如游戏、仿真，甚至作为需要随机过程的算法的一部分。这一能力使开发者能够为他们的应用程序添加不可预测性和多样性，从而增强用户体验和功能性。

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
