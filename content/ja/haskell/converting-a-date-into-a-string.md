---
title:                "Haskell: 日付を文字列に変換する"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換する理由を説明するために、この記事が役立つでしょう。

## 方法
日付を文字列に変換するには、まずは日付を表すデータ型を作成します。例えば、以下のような関数を使って日付を指定した形式で文字列に変換することができます。

```Haskell
import Data.Time.Format

showDate :: Day -> String
showDate = formatTime defaultTimeLocale "%Y/%m/%d"
```

このように、 `formatTime` 関数を使うことで、日付を文字列に変換することができます。例えば、 `showDate (fromGregorian 2020 9 1)` のように使用することができます。この場合、出力は `2020/09/01` となります。

## ディープダイブ
日付の変換方法についてのより詳細な情報については、[Haskell公式ドキュメント](https://www.haskell.org/haddock/)や[Data.Time.Formatモジュールのドキュメント](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)を参考にしてください。また、日付を操作する際には、他の便利な関数やライブラリを使用することもできますので、ぜひ調べてみてください。

## 関連情報を見る
* [Haskell公式ドキュメント](https://www.haskell.org/)
* [Data.Time.Formatモジュールのドキュメント](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)
* [fromGregorian関数のドキュメント](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar-Julian.html#v:fromGregorian)