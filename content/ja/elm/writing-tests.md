---
title:                "Elm: テストを書く"
simple_title:         "テストを書く"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/writing-tests.md"
---

{{< edit_this_page >}}

## なぜテストを書くのか

テストを書くことは、自分のコードが正しく機能することを確認するだけでなく、将来の変更や修正が行われた際にも問題なく動作することを保証することができます。また、テストを書くことによって、コードの品質や保守性を向上させることができます。

## どのように書くのか

まず、Elmテストを書くためには、elm-testパッケージをインストールする必要があります。次に、テストファイルを作成し、テストコードを書きます。

```Elm
import Expect
import ExampleCode exposing (add)

addTest : Test
addTest =
    describe "Add function"
        [ test "Adds two numbers correctly" <|
            \() ->
                Expect.equal (add 2 3) 5
        ]

```

テストコードは、該当する関数が想定通りの結果を返すかどうかをassertion（アサーション）するように書きます。上記の例では、add関数に対して2と3を渡したときに、返り値が5であることを期待しています。

テストファイルを作成したら、コンソールから`elm-test`コマンドを実行することでテストが実行され、結果が表示されます。

## テストを深堀りする

テストを書く際には、データの構造やコードのロジックを理解する必要があります。また、カバレッジツールを使用することで、テストがカバーしていない領域を見つけることができ、より充実したテストを書くことができます。

## もっと詳しく知りたい方は

- [Elmテストのドキュメンテーション](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [テスト駆動開発とは](https://qiita.com/5ltr/items/3796b790818af8ee9eba)
- [カバレッジツールの使い方](https://guide.elm-lang.jp/coverage/)