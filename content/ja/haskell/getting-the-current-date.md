---
title:                "Haskell: 「現在の日付を取得する」"
simple_title:         "「現在の日付を取得する」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ
Haskellを使って現在の日付を取得する必要性があるかご存知ですか？今回は、なぜそれが重要かをお話ししましょう。

##方法
日付を取得する方法は、Haskellでは簡単です。まず、```import Data.Time```というコードで、日付を扱うためのData.Timeモジュールをインポートします。

次に、```getCurrentTime```関数を使用し、現在の日付を取得します。例えば、今日の日付を取得する場合は、次のようなコードになります。

```Haskell
import Data.Time
main = do
    let today = getCurrentTime
    print today
```

上記のコードを実行すれば、簡単に現在の日付が取得できます。

## ディープダイブ
Haskellでは、```getCurrentTime```関数によって取得された日付は、特定のデータ型で表されます。これは、時間、日付、時差などの情報を含んでいます。また、このデータ型内の値を使って、さまざまな日付の計算を行うこともできます。

例えば、```getCurrentTime```関数で取得した日付の2日前の日付を求める方法は以下のようになります。

```Haskell
import Data.Time
main = do
    let today = getCurrentTime
        twoDaysAgo = today - 2
    print twoDaysAgo
```

このように、Haskellでは日付の取得だけでなく、日付の計算も簡単に行うことができます。

## はじめよう
現在の日付を取得するための方法を学びましたが、もしもっと詳しく知りたい場合は、[Haskell公式ドキュメント](https://haskell.org/documentation)や[Real World Haskell](https://haskell.org/)を参考にすると良いでしょう。

## 参考リンク
- [Haskell公式ドキュメント](https://haskell.org/documentation)
- [Real World Haskell](https://haskell.org/)