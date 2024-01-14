---
title:    "Elm: テストの書き方"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elm/writing-tests.md"
---

{{< edit_this_page >}}

## なぜテストを書くのか

プログラマーにとってテストを書くことは非常に重要です。テストを書くことにより、自分のコードがどれだけ頑健であるかを確認することができます。また、新しい機能を追加する際に既存のコードが意図した通りに動作するかを確認することもできます。

## テストの書き方

テストを書くためには、まず最初にElmのテストモジュールをインポートする必要があります。次に、テストを書きたい関数を定義します。そして、テストを書くための特別な関数を使用して、関数の期待される結果を指定します。最後に、作成したテストをテストスイートに追加します。

```Elm
import Test exposing (..)

add : Int -> Int -> Int
add x y =
  x + y

addTest : Test
addTest =
  test "add関数は期待通りに動作する" (
    add 2 3
    |> Expect.equal 5
  )

suite : Test
suite =
  describe "add関数" [
    addTest
  ]
```

上記の例では、`add`関数をテストするために`addTest`という名前のテストを定義しています。そして、`Expect.equal`関数を使用して、`add`関数の結果が`5`であることを期待しています。最後に、定義したテストを`suite`に追加し、全てのテストを実行します。

## テストの詳細

テストを書く際には、様々なアサーションを使用することができます。例えば、`Expect.equal`以外にも、`Expect.notEqual`や`Expect.lt`などがあります。また、テストの実行前に特定の条件が満たされているかどうかを確認することができる`Test.andThen`や、エラーメッセージをカスタマイズすることができる`Expect.custom`などもあります。

## その他のリンク

- [Elmのテストについての公式ドキュメント](https://guide.elm-lang.org/testing/)
- [テストを書く際のベストプラクティス](https://thoughtbot.com/blog/elm-testing-for-beginners)
- [Elmのテストに関する問題を解決する方法](https://medium.com/@matsimitsu/problem-solved-elm-testing-tools-764795d265cc)