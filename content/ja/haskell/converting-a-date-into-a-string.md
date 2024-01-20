---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ?

日付を文字列に変換するとは、特定の日付情報を人間が読み取れる形式の文字列（例：'2022-12-25'）に変換することを言います。これは、日付データをユーザーフレンドリーな形で表示したり、日付フォーマットが必要なAPIにデータを送信するために用いられます。

## どうやって:

以下は日付を文字列に変換するHaskellのサンプルコードです。

```Haskell
import Data.Time.Clock
import Data.Time.Format

convertDateToString :: UTCTime -> String
convertDateToString time = formatTime defaultTimeLocale "%Y-%m-%d" time
```

実行結果は下記の通りです。

```Haskell
Prelude> now <- getCurrentTime
Prelude> convertDateToString now
"2022-12-25"
```

## 深掘り：

1. 過去の文脈：元々UNIXシステムでは、エポックタイム（1970-01-01からの経過秒数）が使用されていましたが、人間が直感的に理解するには非常に手間がかかるため、人間が読めるフォーマットに変換する必要がありました。

2. 代替手段：他の言語では日付のフォーマット変換に`strftime`関数がよく使われます。しかし、Haskellでは`formatTime`関数が提供されています。

3. 実装の詳細：Haskellでは`Data.Time.Format`モジュールが時間のフォーマットに使われます。`formatTime`関数は第一引数にLocale、第二引数にフォーマット、第三引数に時間を取ります。

## 参考情報:

Haskellの日付と時刻に関しては以下のリンクが役立ちます。

1. [Haskellの時間と日付](http://learnyouahaskell.com/input-and-output#dates-and-times)

2. [HaskellのformatTime関数のドキュメンテーション](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html#g:1) 

あまり長くしないで、ポイントを抑えたスタイルは役に立つと確信しています。日付の文字列変換についての理解が深まることを願っています。ありがとうございました！