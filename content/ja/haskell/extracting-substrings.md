---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となぜ ?

部分文字列の抽出は、大きな文字列から任意のサブセット（部分文字列）を取得することです。これは、データ解析やテキスト処理において特定の情報を見つけ出すのに役立ちます。

## 使い方 :

以下のコードを見てみましょう。

```Haskell
import Data.List.Split

substring :: Int -> Int -> String -> String
substring start end = unwords . take (end-start+1) . drop start . words
```

上記コードを実行すると、例えば、

```Haskell
main :: IO ()
main = print(substring 2 4 "Hello world from Haskell")
```

この実行結果は `"world from Haskell"` です。

## 深堀り :

- 歴史的文脈 :
部分文字列の抽出は古くからあるテキスト処理の一部であり、数多くのプログラミング言語でサポートされています。

- 代替手段 :
Haskellの`Data.List.Split`ライブラリは非常に多機能であり、強力なテキスト処理機能を提供しています。しかし、特別な理由がなければ、上記の関数のような自作の関数でも十分なことが多いです。

- 実装詳細 :
`substring`関数は、単純なリスト操作（`drop`と`take`）を利用しています。まず`drop`で必要な開始位置まで要素をスキップし、その後`take`で必要な数の要素を取得します。

## 他にも :

Haskellについてさらに学びたい方は、以下のリンクをご参照ください：

- Haskellの公式チュートリアル : [https://www.haskell.org/tutorial/](https://www.haskell.org/tutorial/)
- 抽出部分文字列の例 : [https://stackoverflow.com/questions/18892281/substring-in-haskell](https://stackoverflow.com/questions/18892281/substring-in-haskell)