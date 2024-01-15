---
title:                "現在の日付を取得する"
html_title:           "Haskell: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ？

誰もが日常的に日付を把握する必要があります。Haskellを使えば、簡単に現在の日付を取得できるため、プログラマーにとっても便利です。

## 方法

まず、Haskellの標準ライブラリである`Data.Time`をインポートします。

```Haskell
import Data.Time
```

次に、`getCurrentTime`関数を使って現在の日時を取得し、`UTCTime`型の値として保存します。

```Haskell
currentDate <- getCurrentTime
```

そして、`formatTime`関数を使って、取得した日時を任意のフォーマットに変換することができます。例えば、`YYYY-MM-DD`の形式で表示するためには、次のように`formatTime`関数を使います。

```Haskell
let formattedDate = formatTime defaultTimeLocale "%Y-%m-%d" currentDate
```

最後に、`formattedDate`を使って、現在の日付を出力します。

```Haskell
putStrLn formattedDate
```

実行すると、次のような出力が得られます。

```
2021-10-10
```

## ディープダイブ

`getCurrentTime`関数は、`IO UTCTime`という型を返します。これは、入出力を伴う操作であることを示しています。また、`UTCTime`型は、世界中で同じ時間を表現するために使用されるグリニッジ標準時（UTC）を使用します。これは、国や地域に依存しない普遍的な日付と時刻を表現するために便利です。

また、`formatTime`関数は、`UTCTime`型の値を受け取り、任意のフォーマットに変換するために使用されます。`defaultTimeLocale`を使用することで、言語や地域に依存しないデフォルトのフォーマットが使用されますが、必要に応じてカスタマイズすることもできます。

## もっと詳しく知りたい方は

- [Haskellのドキュメント：Date and Time](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/time-1.9.4/Data-Time.html)
- [Hackage：Data.Time](https://hackage.haskell.org/package/time-1.9.4/docs/Data-Time.html)
- [Haskell Wiki：Date and Time](https://wiki.haskell.org/Date_and_time)

## 関連リンク

- [Markdown記法の基本](https://qiita.com/tbpgr/items/989c6badefff69377da7)
- [はてなブログ：Markdown記法入門](https://hatenablog.com/guide/basic/markdown)