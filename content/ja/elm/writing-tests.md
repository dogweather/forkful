---
title:                "テストの作成"
aliases:
- ja/elm/writing-tests.md
date:                  2024-02-03T19:30:55.743817-07:00
model:                 gpt-4-0125-preview
simple_title:         "テストの作成"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

Elmでテストを書くことは、Elmのコードの正確性を検証するテストケースを作成し、期待通りに動作することを確認することを含みます。プログラマーは、早期にバグを発見し、メンテナンスを容易にし、アプリケーションの品質と信頼性を向上させるためにこれを行います。

## 方法：

Elmでは、単体テストとファズテストの作成に`elm-explorations/test`パッケージを使用します。プロジェクトにパッケージを追加することから始めます：

```elm
elm install elm-explorations/test
```

テストファイルを作成します。例えば`tests/ExampleTest.elm`とし、テストモジュールをインポートします。ここでは、関数`add : Int -> Int -> Int`を検証する簡単なテストを示します：

```elm
module ExampleTest exposing (..)

import Expect
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "A simple addition function"
        [ test "Adding 2 and 3 yields 5" <| 
            \_ -> add 2 3 |> Expect.equal 5
        ]

```

テストを実行するには、`elm-test`が必要になります：

```shell
npm install -g elm-test
elm-test
```

これにより、テストがコンパイルされ、結果がターミナルに出力されます。上記の例では、出力は次のようになるはずです：

```
TEST RUN PASSED

Duration: 42 ms
Passed:   1
Failed:   0
```

より複雑な例として、`add`関数が幅広い整数入力を正しく処理できるかどうかをファズテストで確認したいとします。その場合、`ExampleTest.elm`を次のように変更します：

```elm
module ExampleTest exposing (..)

import Expect
import Fuzz exposing (int)
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Testing add with fuzzing"
        [ fuzz int "Fuzz testing add with random ints" <| 
            \int1 int2 -> add int1 int2 |> Expect.equal (int1 + int2)
        ]
```

再び`elm-test`を実行して、ファズテストを動作させます。出力はランダムな入力によって変わりますが、成功したテストは失敗がないことを示します：

```
TEST RUN PASSED

Duration: 183 ms
Passed:   100
Failed:   0
```

これらの例は、`elm-explorations/test`パッケージを使用して、Elmで単純な単体テストとファズテストを書き、実行する方法を示しています。テストは開発プロセスの重要な部分であり、Elmアプリケーションが信頼でき、高品質を維持するのに役立ちます。
