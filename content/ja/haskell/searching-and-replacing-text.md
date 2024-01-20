---
title:                "テキストの検索と置換"
html_title:           "Java: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 何？そして、なぜ？
テキストの検索・置換は、特定の文字列を見つけて新しい文字列に置き換えるプロセスです。プログラムの内部でデータを操作したり、ユーザーからの入力を適切に処理したりするために、プログラマーがよく使います。

## どうやって：
Haskellでは`Data.List.Utils`の`replace`関数で文字列を置き換えることができます。ただし、この関数を使う前に`Data.List.Utils`をインストールする必要があります。

```Haskell
import Data.List.Utils

main = do
  let originalText = "Hello, World!"
  let newText = replace "World" "Haskell" originalText
  print newText
```

このコードは "Hello, Haskell!" を出力します。

## ますます深く：
**歴史的な文脈:** テキストの検索と置換はEDI(エレクトロニック・データ・インタチェンジ)では古くから一般的でした。これは後にテキストエディタやワードプロセッサー、更にはプログラミング言語にも導入されました。

**代替案:** Haskellでは他にも`subRegex`関数を使って正規表現を使用した置換を行うこともあります。

**実装の詳細:**`replace`関数の内部では二つのリストを比較しています。もしリストが一致すれば、置換文字列に変え、一致しなければそのままにしています。

## 参照：
1. ["Haskell Text" パッケージ](https://hackage.haskell.org/package/text)