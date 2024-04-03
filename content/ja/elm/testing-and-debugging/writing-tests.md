---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:55.743817-07:00
description: "\u65B9\u6CD5\uFF1A Elm\u3067\u306F\u3001\u5358\u4F53\u30C6\u30B9\u30C8\
  \u3068\u30D5\u30A1\u30BA\u30C6\u30B9\u30C8\u306E\u4F5C\u6210\u306B`elm-explorations/test`\u30D1\
  \u30C3\u30B1\u30FC\u30B8\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B8\
  \u30A7\u30AF\u30C8\u306B\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u8FFD\u52A0\u3059\u308B\
  \u3053\u3068\u304B\u3089\u59CB\u3081\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.013133-06:00'
model: gpt-4-0125-preview
summary: "Elm\u3067\u306F\u3001\u5358\u4F53\u30C6\u30B9\u30C8\u3068\u30D5\u30A1\u30BA\
  \u30C6\u30B9\u30C8\u306E\u4F5C\u6210\u306B`elm-explorations/test`\u30D1\u30C3\u30B1\
  \u30FC\u30B8\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B8\u30A7\u30AF\
  \u30C8\u306B\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u8FFD\u52A0\u3059\u308B\u3053\u3068\
  \u304B\u3089\u59CB\u3081\u307E\u3059\uFF1A."
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

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
