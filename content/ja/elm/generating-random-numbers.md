---
title:                "乱数の生成"
date:                  2024-01-27T20:33:49.114260-07:00
model:                 gpt-4-0125-preview
simple_title:         "乱数の生成"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？
Elmで乱数を生成することは、ゲーム、シミュレーション、セキュリティアルゴリズムなどのアプリケーションに不可欠な予測不可能な数値を作り出すことを意味します。プログラマーは、現実世界の変動をシミュレートしたり、ユーザー体験を向上させたり、暗号化技術を用いてデータを安全に保つために、乱数を使用します。

## どのように：
Elmは他の多くのプログラミング言語とは異なり、関数を純粋に保つシステムを利用して乱数を扱います。乱数を生成するには、Elmの `Random` モジュールを利用する必要があります。ここに、1から100までの乱数を生成する基本的な例を示します：

```Elm
import Html exposing (Html, text)
import Random

main : Html msg
main =
    Random.generate NewRandomNumber (Random.int 1 100)
    |> Html.map (text << toString)

type Msg = NewRandomNumber Int
```

このスニペットは `Random.generate` を使用して、実行されると指定された範囲内で乱数を生成するコマンドを作成します。 `type Msg` 宣言は、Elmアプリケーションの更新関数で生成された数値を扱うために使用されます。

もう少しインタラクティブな例として、ユーザーがクリックを通じて乱数生成をトリガーするシナリオを見てみましょう：

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

このElmアプリケーションは、ユーザーがボタンをクリックするたびに新しい乱数で表示を更新するインタラクティビティを導入しています。

## 深堀り
Elmの乱数生成システムの設計は、言語の純粋性と予測可能性へのコミットメントに由来しています。各呼び出しで異なる値を返す直接的な、純粋でない関数の代わりに、Elmは乱数を `Cmd` 構造でカプセル化し、副作用を純粋な関数から分離するそのアーキテクチャと一致させます。

このアプローチは、アプリケーションの振る舞いの一貫性を保証し、デバッグを容易にする一方で、乱数を命令形で生成するのに慣れている人には学習曲線を導入します。しかし、アプリケーションの純粋性を維持し、テストの容易さは、最初の複雑さをしばしば上回る利点です。

Elmの方法は、グローバル乱数生成器を提供する言語とも対照的で、共有状態による微妙なバグを引き起こす可能性があります。乱数生成とその効果の明示的な処理を要求することにより、Elmは開発者がアプリケーションにおける乱数がどのように、そしてどこで影響を与えるかについてより批判的に考えるよう促し、より堅牢で予測可能なコードにつながります。

代替手段として、他の関数型言語は類似の機能を提供していますが、異なる方法で実装する場合があります。例えば、Haskellも乱数生成の純粋性を維持していますが、モナドという、Elmが意図的に避ける概念を使用しています。比較的に、Elmのアプローチは新参者にとってよりアクセスしやすく、関数型プログラミングの原則の力を犠牲にすることなく、直接的なアプリケーションアーキテクチャを強調しています。
