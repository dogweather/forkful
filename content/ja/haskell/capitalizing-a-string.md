---
title:                "Haskell: 文字列の大文字変換"
simple_title:         "文字列の大文字変換"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列を大文字にすることに興味がある人にとって、Haskellのプログラミングは非常に魅力的なものです。文字列を大文字に変換することは、エンコーディングやフォーマットなどの多くの問題を解決することができます。

## 方法

まずは、必要な言語拡張を有効にしましょう。

```Haskell
{-# LANGUAGE OverloadedStrings #-}
```

次に、Data.Textモジュールをインポートし、capitalize関数を定義します。

```Haskell
import Data.Text (toUpper)

capitalize :: Text -> Text
capitalize str = toUpper str
```

最後に、入力文字列を受け取って、大文字に変換した文字列を返すように関数を呼び出します。

```Haskell
capitalize "hello world" -- 出力: "HELLO WORLD"
```

## ディープダイブ

文字列を大文字に変換する際に、多くの人が便利だと感じるのは、特殊文字や記号などを含む文字列でも正しく変換することができる点です。HaskellのData.Textモジュールには、これらの文字を扱うための便利な関数がたくさん用意されています。また、文字列を大文字に変換するだけでなく、Data.Textモジュールでは様々な文字列操作を行う関数が提供されています。

## 参考リンク

- Data.Textモジュールの公式ドキュメント: https://hackage.haskell.org/package/text/docs/Data-Text.html
- Haskell言語のチュートリアル: https://www.haskell.org/tutorial/
- 他の便利な言語拡張について: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/index.html

## 参考

- [Haskellでの文字列操作について](https://qiita.com/hiratara/items/7686b334c35194f9cb5d)
- [実践的なHaskellプログラミング入門](https://blog.eleven-labs.com/ja/introduction-a-la-programmation-haskell-pratique/)