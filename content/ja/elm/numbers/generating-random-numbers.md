---
changelog:
- 2024-02-27, dogweather, edited and tested
- 2024-02-27, gpt-4-0125-preview, translated from English
date: 2024-02-27 22:50:37.210043-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.001864-06:00'
model: gpt-4-0125-preview
summary: "Elm\u3067\u4E71\u6570\u3092\u751F\u6210\u3059\u308B\u306B\u306F\u3001`Random`\u30E2\
  \u30B8\u30E5\u30FC\u30EB\u3092\u4F7F\u7528\u3057\u3066\u7591\u4F3C\u4E71\u6570\u3092\
  \u751F\u6210\u3057\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30B2\u30FC\u30E0\u3001\
  \u30B7\u30DF\u30E5\u30EC\u30FC\u30B7\u30E7\u30F3\u3001\u307E\u305F\u306F\u78BA\u7387\
  \u7684\u30D7\u30ED\u30BB\u30B9\u304C\u5FC5\u8981\u306A\u30A2\u30EB\u30B4\u30EA\u30BA\
  \u30E0\u306E\u4E00\u90E8\u3068\u3057\u3066\u3001\u3055\u307E\u3056\u307E\u306A\u30BF\
  \u30B9\u30AF\u306B\u4FBF\u5229\u3067\u3059\u3002\u3053\u306E\u6A5F\u80FD\u306B\u3088\
  \u308A\u3001\u958B\u767A\u8005\u306F\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\
  \u306B\u4E88\u6E2C\u4E0D\u53EF\u80FD\u3055\u3068\u591A\u69D8\u6027\u3092\u8FFD\u52A0\
  \u3067\u304D\u3001\u30E6\u30FC\u30B6\u30FC\u4F53\u9A13\u3068\u6A5F\u80FD\u6027\u3092\
  \u5411\u4E0A\u3055\u305B\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002."
title: "\u4E71\u6570\u306E\u751F\u6210"
weight: 12
---

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
