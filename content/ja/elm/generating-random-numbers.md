---
title:    "Elm: ランダムナンバーを生成する"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## なぜ

ランダムな数字の生成に取り組む理由は何でしょうか？結論から言うと、プログラムでランダムさを実現することで、プログラムの複雑性を増し、面白いものを作ることができるからです。

## 生成方法

```elm
import Random exposing (..)

type alias Model = 
    { randomNum : Int
    }

type Msg = 
    GenerateRandomNum

init : Model
init = 
    { randomNum = 0
    }

update : Msg -> Model -> Model
update msg model = 
    case msg of 
        GenerateRandomNum -> 
            model |> 
            Random.generate randomGenerator 
            |> 
            Task.attempt RandomNumGenerated


randomGenerator : Generator Msg
randomGenerator = 
    randomInt 1 100 
    |> 
    Random.map GenerateRandomNum


view : Model -> Html Msg
view model = 
    div [] 
        [ h1 [] [ text (String.fromInt model.randomNum) ]
        , button [ onClick GenerateRandomNum ] [ text "Generate Random Number" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
```

上記のコードでは、ランダムな数字を生成するために`Random`モジュールをインポートし、新しいモデルにランダムな数字を格納するための`randomNum`フィールドを追加します。次に、`GenerateRandomNum`というメッセージを定義し、`Random.generate`関数を使用してランダムな数字を生成するためのタスクを定義します。また、`main`関数では、`RandomNumGenerated`というメッセージを受け取って`randomNum`を更新するように定義します。最後に、`view`関数では`GenerateRandomNum`ボタンを表示し、押下することでランダムな数字が更新されるようにします。

## より深い掘り下げ

`Random`モジュールには、プログラムでより複雑なランダムさを実現するための様々な関数が用意されています。それらの関数を組み合わせることで、想像を超えるようなランダムさを実現することができます。また、`Random`モジュールはシード値を受け取ることで、同じシード値を使用することで同じ結果を得ることも可能です。

## 参考リンク

- [Elmでランダムな数字を生成する方法](https://guide.elm-lang.jp/architecture/random/)
- [Randomモジュールの公式ドキュメント](https://package.elm-lang.org/packages/elm/random/latest/)
- [Elmでランダムなデータを生成する方法の紹介](https://qiita.com/8RAz_H1LUm/items/a887c8e0174ee79af84f)
- [Elmでランダムな色を生成する方法](https://particulae.com/random-colors-in-elm/)
- [ランダムな数字生成デモプログラム](https://ellie-app.com/82Rw88r6WGca1)