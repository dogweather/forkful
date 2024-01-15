---
title:                "テストを書く"
html_title:           "Elm: テストを書く"
simple_title:         "テストを書く"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/writing-tests.md"
---

{{< edit_this_page >}}

## なぜ

テストを書くことで、プログラムの品質を向上させることができます。コードのバグやエラーを事前に発見し、より信頼性の高いソフトウェアを作ることができるため、開発者にとって欠かせない作業です。

## 作り方

まずは、[Elmテストパッケージ](https://package.elm-lang.org/packages/elm-explorations/test/latest/)をインストールしましょう。その後、テストファイルを作成し、テストしたい関数やモジュールを`import`します。そして、各関数や振る舞いに対してテストを作成します。以下は例です。

```Elm
import Test exposing (..)
import Expect exposing (expect)

-- テストする関数
square : Int -> Int
square x =
    x * x

-- `test`関数は関数名やテストの内容を表す
tests : Test
tests =
    describe "square" [ 
        test "2を渡したら4を返す" (
            expect (square 2)
                |> toEqual 4
        ),
        test "3を渡したら9を返す" (
            expect (square 3)
                |> toEqual 9
        )
    ]

```

実行すると、以下のような結果が得られます。

```
Passed: square
    - 2を渡したら4を返す
    - 3を渡したら9を返す
```

## 深堀り

テストを書く際には、どのようなケースに対してテストを書くか、どのような入力を与えるかなどを考えることが重要です。また、より複雑なテストを行う際には、[Generators](https://package.elm-lang.org/packages/elm-community/elm-test-extra/latest/Generators)を使用することで、ランダムな入力値を生成することができます。

また、テストコードもメンテナンスする必要があります。コードの変更や機能追加を行った際には、テストを再実行し、予期せぬ変更がないかを確認することが大切です。

## 関連リンク

- [Elmテストパッケージ](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Generators](https://package.elm-lang.org/packages/elm-community/elm-test-extra/latest/Generators)
- [テストの書き方 入門編 - Qiita](https://qiita.com/kosuke-nakai/items/8d2b61b71ae32d1574f6)