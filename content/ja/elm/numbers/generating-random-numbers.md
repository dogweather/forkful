---
changelog:
- 2024-02-27, dogweather, edited and tested
- 2024-02-27, gpt-4-0125-preview, translated from English
date: 2024-02-27 22:50:37.210043-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Elm\u306E\u7D14\u7C8B\
  \u95A2\u6570\u7684\u306A\u6027\u8CEA\u306F\u3001\u547D\u4EE4\u578B\u8A00\u8A9E\u3067\
  \u53EF\u80FD\u306A\u3088\u3046\u306B\u76F4\u63A5\u4E71\u6570\u3092\u751F\u6210\u3067\
  \u304D\u306A\u3044\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u4EE3\u308F\
  \u308A\u306B\u3001`Random`\u30E2\u30B8\u30E5\u30FC\u30EB\u3092\u547D\u4EE4\u3068\
  \u7D44\u307F\u5408\u308F\u305B\u3066\u4F7F\u7528\u3057\u307E\u3059\u3002\u3053\u3053\
  \u3067\u306F\u30011\u304B\u3089100\u306E\u9593\u306E\u30E9\u30F3\u30C0\u30E0\u306A\
  \u6574\u6570\u3092\u751F\u6210\u3059\u308B\u57FA\u672C\u7684\u306A\u4F8B\u3092\u793A\
  \u3057\u307E\u3059\u3002 \u307E\u305A\u3001`elm install\u2026"
lastmod: '2024-03-13T22:44:42.001864-06:00'
model: gpt-4-0125-preview
summary: "Elm\u306E\u7D14\u7C8B\u95A2\u6570\u7684\u306A\u6027\u8CEA\u306F\u3001\u547D\
  \u4EE4\u578B\u8A00\u8A9E\u3067\u53EF\u80FD\u306A\u3088\u3046\u306B\u76F4\u63A5\u4E71\
  \u6570\u3092\u751F\u6210\u3067\u304D\u306A\u3044\u3053\u3068\u3092\u610F\u5473\u3057\
  \u307E\u3059\u3002\u4EE3\u308F\u308A\u306B\u3001`Random`\u30E2\u30B8\u30E5\u30FC\
  \u30EB\u3092\u547D\u4EE4\u3068\u7D44\u307F\u5408\u308F\u305B\u3066\u4F7F\u7528\u3057\
  \u307E\u3059\u3002\u3053\u3053\u3067\u306F\u30011\u304B\u3089100\u306E\u9593\u306E\
  \u30E9\u30F3\u30C0\u30E0\u306A\u6574\u6570\u3092\u751F\u6210\u3059\u308B\u57FA\u672C\
  \u7684\u306A\u4F8B\u3092\u793A\u3057\u307E\u3059."
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
