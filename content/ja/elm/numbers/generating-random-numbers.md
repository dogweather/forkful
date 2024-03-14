---
changelog:
- 2024-02-27, dogweather, edited and tested
- 2024-02-27, gpt-4-0125-preview, translated from English
date: 2024-02-27 22:50:37.210043-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.001864-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u4E71\u6570\u306E\u751F\u6210"
---

{{< edit_this_page >}}

## 何となぜ？
Elmで乱数を生成するには、`Random`モジュールを使用して疑似乱数を生成します。これは、ゲーム、シミュレーション、または確率的プロセスが必要なアルゴリズムの一部として、さまざまなタスクに便利です。この機能により、開発者はアプリケーションに予測不可能さと多様性を追加でき、ユーザー体験と機能性を向上させることができます。

## どのようにして：
Elmの純粋関数的な性質は、命令型言語で可能なように直接乱数を生成できないことを意味します。代わりに、`Random`モジュールを命令と組み合わせて使用します。ここでは、1から100の間のランダムな整数を生成する基本的な例を示します。

まず、`elm install elm/random`で`Random`モジュールをインストールします。次に、必要なHTMLおよびイベントモジュールと共に、Elmファイルにインポートします。

`src/Main.elm`

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, text, div)
import Html.Events exposing (onClick)
import Random
```

これが自己完結型の例であるために、このボイラープレートを追加できます：

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

次に、ランダムな数字を生成する**コマンド**を定義します。これには、生成されたランダムな数字を処理するための`Msg`型の設定、それを格納する`Model`、そしてそれらをすべて結びつける更新関数が含まれます。

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

数字生成をトリガーするには、例えばビュー内のボタンを通じて`Generate`メッセージを送信します：

```elm
view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Random Number: " ++ String.fromInt model.randomNumber) ]
        , button [ onClick Generate ] [ text "Generate" ]
        ]
```

"Generate"ボタンをクリックすると、1から100の間のランダムな数字が表示されます。

この単純なアプローチは、他の関数を利用して`Random`モジュールによりランダムな浮動小数点数、リスト、またはカスタムタイプに基づいた複雑なデータ構造を生成することにより適応され拡張され、Elmアプリケーションへの予測不可能さの追加のための広大な遊び場を提供します。

Elmガイドでは、より詳細な情報が記載されています。また、[六面体のサイコロを振る例](https://guide.elm-lang.org/effects/random)もあります。
