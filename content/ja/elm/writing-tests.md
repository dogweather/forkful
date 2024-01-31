---
title:                "テストの作成"
date:                  2024-01-19
html_title:           "Bash: テストの作成"
simple_title:         "テストの作成"

category:             "Elm"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?（なに？どうして？）
テストコードとは、プログラムが正しい動作をするかチェックするためのコードです。バグを見つけ、機能が予期される通りに動くことを保証するためにプログラマーはテストを書きます。

## How to:（やり方）
Elmでのテストコード例を示します。

```Elm
import Expect exposing (Expectation)
import Test exposing (..)
import String

suite : Test
suite =
    describe "String tests"
        [ test "Length of 'Elm'" <| \_ ->
            "Elm" |> String.length |> Expect.equal 3
        , test "Replace character 'e' with 'a'" <| \_ ->
            "Hello Elm" |> String.replace "e" "a" |> Expect.equal "Hallo Elm"
        ]

-- テストを実行すると以下のような出力になります。
-- TEST RUN PASSED
--
-- String tests
--     ✓ Length of 'Elm'
--     ✓ Replace character 'e' with 'a'
--
-- 2 tests run, passed: 2, failed: 0
```

## Deep Dive（掘り下げ）
Elmでテストを書く歴史はまだ浅いですが、Elm-testパッケージは Elm のためのデ・ファクト標準テストフレームワークとなっています。他言語のテストフレームワークと比較して、Elm-testは型安全性と読みやすさに重点を置いています。内部的には、ランダムなテストケースの生成とテストの組み合わせを行うことができる「fuzz testing」をサポートしています。

## See Also（関連する情報源）
- Elm-testの公式ドキュメント: [https://package.elm-lang.org/packages/elm-explorations/test/latest/](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- Fuzz testingに関する解説記事: [https://elmprogramming.com/fuzz-testing.html](https://elmprogramming.com/fuzz-testing.html)
