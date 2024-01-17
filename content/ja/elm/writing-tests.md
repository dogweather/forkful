---
title:                "テストの作成"
html_title:           "Elm: テストの作成"
simple_title:         "テストの作成"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/writing-tests.md"
---

{{< edit_this_page >}}

## 何をしているの？ ＆ なぜするの？
テストを書くとは、簡単に言えば自分が書いたコードが正しく動作するかどうかを確認することです。プログラマーはテストを書くことで、自分のコードが意図したとおりに動くことを確実にすることができます。

## 方法：
```Elm
import Test
import Expect

add : Int -> Int -> Int
add x y =
  x + y

test : Test
test =
  Test.test "Testing addition" [
    Expect.equal (add 2 3) 5
  ]

main : Test.Runner.Config -> Html.Html
main testConfig =
  Html.div [] [
    Test.Runner.runTest test testConfig
  ]
```

上記のコードは、`add` 関数をテストするための簡単なテストコードの例です。テストを書くためには、`Test` モジュールと `Expect` モジュールをインポートしなければなりません。`add` 関数をテストするために、`Expect.equal` 関数を使用します。最後に、`main` 関数でテストを実行し、結果を表示します。

## 詳細について：
テストを書くことは、品質保証のために非常に重要です。過去には、テストを書くための別の方法として、デバッガーやログを使用してコードをテストするという方法がありましたが、これらの方法では手動の確認が必要であり、時間もかかります。そのため、テストフレームワークが開発され、現在ではテストを書くことがより簡単かつ効率的になりました。Elmでは、そのようなテストフレームワークの一つとして `elm-test` が提供されています。

## 関連情報：
- 公式ドキュメント： https://guide.elm-lang.jp/test/ 
- テストの書き方の例：https://github.com/elm-examples/elm-test-examples