---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
文字列の長さを見つけるとは、文字列の中にある文字の数を特定することです。プログラマーがこれを行う理由は多岐にわたりますが、主に文字列の操作や分析に役立つからです。

## 方法：
Elmでは、`String.length`関数を用いて文字列の長さを得ることができます。たとえば：

```Elm
import Html exposing (text)

main =
    text (String.length "こんにちは")
```

出力結果は5になります。

## 深層探訪
文字列の镄さを取得するというアイデアは、プログラミングの早い段階から存在していました。この基本操作は、文字列の比較、検索など、さまざまな文字列操作の基礎です。

Elmの`String.length`は、Unicodeのコードポイントを数えるため、サロゲートペアを考慮に入れて正確な長さを返します。

この関数の代替として、自分で関数を作成して文字列をループすることもできますが、`String.length`が最も直接的で効率的です。

## 関連情報
以下のリンクでさらに情報を得ることができます：

- [Elmの公式ドキュメンテーション](https://package.elm-lang.org/packages/elm/core/latest/String#length)

以上がElmで文字列の長さを探す方法についての記事です。この情報が役立つことを願っています。