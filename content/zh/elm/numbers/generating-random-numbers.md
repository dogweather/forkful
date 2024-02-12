---
title:                "生成随机数"
aliases:
- /zh/elm/generating-random-numbers/
date:                  2024-01-27T20:33:35.854123-07:00
model:                 gpt-4-0125-preview
simple_title:         "生成随机数"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
在Elm中生成随机数涉及到创建不可预测的数值，这对于游戏、仿真和安全算法等应用至关重要。程序员使用随机性来模拟现实世界的变化性，增强用户体验，或使用加密技术保护数据。

## 如何操作:
Elm处理随机性的方式与许多编程语言不同，它利用一个系统来保持函数的纯净性。要生成随机数，你必须使用Elm的`Random`模块。这里有一个生成1到100之间随机数的基本示例：

```Elm
import Html exposing (Html, text)
import Random

main : Html msg
main =
    Random.generate NewRandomNumber (Random.int 1 100)
    |> Html.map (text << toString)

type Msg = NewRandomNumber Int
```

这段代码使用`Random.generate`创建了一个命令，该命令在执行时会在指定范围内产生一个随机数。`type Msg`声明用于在Elm应用程序的更新函数中处理生成的数字。

对于更互动的示例，我们看一个通过点击触发生成随机数的场景：

```Elm
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random

type alias Model = Int

type Msg = Generate

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            (model, Random.generate NewRandomNumber (Random.int 1 100))

view : Model -> Html Msg
view model =
    div []
        [ text ("Generated number: " ++ String.fromInt model)
        , button [ onClick Generate ] [ text "Generate new number" ]
        ]

type Msg = NewRandomNumber Int
```

这个Elm应用程序引入了互动性，每次用户点击按钮时都会用新的随机数更新显示。

## 深入探讨
Elm随机数生成系统的设计源于该语言对纯净性和可预测性的承诺。与每次调用返回不同值的直接、不纯净的函数不同，Elm将随机性封装在`Cmd`结构中，与其架构相符，该架构将副作用与纯函数分离。

虽然这种方法保证了应用行为的一致性并便于调试，但它为习惯了命令式生成随机数的人带来了学习曲线。然而，保持应用纯净性和便于测试的好处通常大于初始的复杂性。

Elm的方法也与提供全局随机数生成器的语言形成对比，这可能由于共享状态导致微妙的错误。通过要求显式处理随机数生成及其效果，Elm鼓励开发人员更加深入地思考随机性如何以及在何处影响他们的应用程序，从而导致更健壮和可预测的代码。

对于替代方案，其他函数式语言提供了类似的功能，但可能以不同的方式实现它们。例如，Haskell也在随机数生成中保持纯净性，但通过使用monads来实现，这是Elm为了简化其模型而故意避免的概念。相比之下，Elm的方法对于新手更加容易访问，并强调一个直接的应用架构，而不牺牲函数式编程原则的力量。
