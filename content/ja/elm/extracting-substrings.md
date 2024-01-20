---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となぜ？
文字列から特定の部分文字列を取り出すことをサブストリングといいます。これは某特定の情報を分析したり、検索したり、処理したりする際に使います。

## 使い方
```Elm
substring : Int -> Int -> String -> String
substring start end str =
    String.slice start end str
```
上記の`substring`関数を使えば、開始位置と終了位置を指定して文字列から部分文字列を抽出できます。
```Elm
substring 0 5 "Hello, World!" -- 出力: "Hello"
substring 7 12 "Hello, World!" -- 出力: "World"
```

## 深堀り
かつてElm 0.18では`String`モジュールが`String.substring`関数を提供していましたが、現在のバージョンであるElm 0.19では名前が`String.slice`に変更され、より直感的な名前になりました。

一部の言語では`substring`, `substr`, `slice`など異なる関数で部分文字列を取得できますが、常に引数として開始位置と終了位置（または開始位置と長さ）が必要です。

Elmの`String.slice`関数を実装するには、まず文字列をリストに変換し、適切なスライス操作を行い、再び文字列に変換する必要があります。

## 関連情報
- Elmの公式`String`モジュールのドキュメント: http://package.elm-lang.org/packages/elm/core/latest/String
- Elmの文字列操作についての詳細なチュートリアル: https://elmprogramming.com/strings.html#substring