---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何となぜ?

文字列から特定のパターンに一致する文字を削除する手続きです。データの整形、不要な値の排除など、より有効かつ意図的なデータ操作の一環としてプログラマーが使用します。

## 実装方法

Haskellには `Data.List` の `delete` 関数を使って特定の文字を削除することができます。以下に一例を示します。

```Haskell
import Data.List

deleteChar :: Char -> String -> String
deleteChar c s = (filter (/= c) s)

main = do
    let sentence = "I love Japanese curry."
    let noVowels = deleteChar 'a' sentence
    print noVowels
```

このコードは、与えられた文字列から特定の文字（この例では 'a'）を削除します。出力結果は以下の通りです。

```Haskell
"I love Jpnese curry."
```

## 詳細な解説

Haskellの `Data.List` モジュールで提供されている `filter` 関数を使用して、文字を削除します。この関数は高階関数であり、他の関数（この場合 `/=`）とリスト（または文字列）を引数にとります。元の文字列を参照し、指定した関数によってTrueと評価される各要素の新しいリストを生成します。

代替として、`Char` を `Maybe Char` へ変換する `map` 関数と `catMaybes` 関数を組み合わせて使用する方法もある。ここでは詳しく説明しないが、必要に応じて調査してみよう。

## 関連情報

詳しい情報は以下のリンクから得られます：

- Haskell `filter` 関数のドキュメンテーション： http://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html#v:filter
- 高階関数についての一般的な説明： https://wiki.haskell.org/Higher_order_function
- `Maybe` 型と `catMaybes` 関数についての説明： http://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Maybe.html