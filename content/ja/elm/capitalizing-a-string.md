---
title:                "文字列の先頭を大文字にする"
html_title:           "C: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (なんで？どうして？)
文字列を大文字にすることは、文字列内のすべての文字を大文字に変換するプロセスです。これにより、ユーザーインターフェースでの見出し表示や、データの正規化の際に利用されます。

## How to: (やり方)
Elmには文字列を全部大文字にする組み込み関数はありません。そのため、`String` モジュールと `List` モジュールの関数を組み合わせて実装します。

```Elm
import String exposing (toList, fromList, toUpper)
import List exposing (map)

capitalizeString : String -> String
capitalizeString str =
    str |> toList |> map toUpper |> fromList

-- 使用例
main =
    let
        originalString = "hello, world!"
    in
    Html.text (capitalizeString originalString)

-- 出力: "HELLO, WORLD!"
```

## Deep Dive (探検)
Elmは関数型言語です。`String` を大文字にする関数がないので、文字列を一文字ずつのリストに変換（`toList`）、それぞれの文字を大文字に変換（`map toUpper`）、そして再び文字列に結合します（`fromList`）。

過去、多くのプログラミング言語でこれは一般的な処理でした。Elmはパフォーマンスと可読性を重視するため、このような方法を取ります。

他言語の代替策としては、全ての文字をループ処理で個々に大文字に変換することが挙げられます。

文字列操作において、Elmはコードの安全性と保守性を向上させるため、純粋関数の使用を推奨しています。

## See Also (関連情報)
- Elm String documentation: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)
- Elm List documentation: [https://package.elm-lang.org/packages/elm/core/latest/List](https://package.elm-lang.org/packages/elm/core/latest/List)
- Elmの公式ガイド（関数型プログラミングについて）: [https://guide.elm-lang.org](https://guide.elm-lang.org)
