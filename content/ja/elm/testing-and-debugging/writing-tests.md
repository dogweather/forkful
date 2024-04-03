---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:55.743817-07:00
description: "Elm\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3053\u3068\u306F\u3001\
  Elm\u306E\u30B3\u30FC\u30C9\u306E\u6B63\u78BA\u6027\u3092\u691C\u8A3C\u3059\u308B\
  \u30C6\u30B9\u30C8\u30B1\u30FC\u30B9\u3092\u4F5C\u6210\u3057\u3001\u671F\u5F85\u901A\
  \u308A\u306B\u52D5\u4F5C\u3059\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3059\u308B\u3053\
  \u3068\u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001\u65E9\u671F\u306B\u30D0\u30B0\u3092\u767A\u898B\u3057\u3001\u30E1\u30F3\u30C6\
  \u30CA\u30F3\u30B9\u3092\u5BB9\u6613\u306B\u3057\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\
  \u30B7\u30E7\u30F3\u306E\u54C1\u8CEA\u3068\u4FE1\u983C\u6027\u3092\u5411\u4E0A\u3055\
  \u305B\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.013133-06:00'
model: gpt-4-0125-preview
summary: "Elm\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3053\u3068\u306F\u3001Elm\u306E\
  \u30B3\u30FC\u30C9\u306E\u6B63\u78BA\u6027\u3092\u691C\u8A3C\u3059\u308B\u30C6\u30B9\
  \u30C8\u30B1\u30FC\u30B9\u3092\u4F5C\u6210\u3057\u3001\u671F\u5F85\u901A\u308A\u306B\
  \u52D5\u4F5C\u3059\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u3092\
  \u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u65E9\
  \u671F\u306B\u30D0\u30B0\u3092\u767A\u898B\u3057\u3001\u30E1\u30F3\u30C6\u30CA\u30F3\
  \u30B9\u3092\u5BB9\u6613\u306B\u3057\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\
  \u30F3\u306E\u54C1\u8CEA\u3068\u4FE1\u983C\u6027\u3092\u5411\u4E0A\u3055\u305B\u308B\
  \u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

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
