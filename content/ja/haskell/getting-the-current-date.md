---
title:                "Haskell: 現在の日付を取得する"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

今日の日付を取得する必要がある理由はさまざまです。例えば、プログラムを書いているときに現在の日付を使用する必要があるかもしれません。また、日付を使用してデータを整理する場合にも便利です。

## 方法

現在の日付を取得するには、Haskellの標準ライブラリであるData.Timeモジュールを使用します。下記のコードを実行することで、現在の日付を取得することができます。

```Haskell
import Data.Time

main = do
    now <- getCurrentTime
    let today = utctDay now
    putStrLn $ "今日の日付は " ++ show today ++ " です。"
```

このコードを実行すると、以下のように現在の日付が表示されます。

```
今日の日付は 2021-01-01 です。
```

また、currentDateTime関数を使用することで、現在の日付と時刻を取得することもできます。

## 深堀り

Data.Timeモジュールには、日付や時刻を操作するための様々な関数が用意されています。例えば、Date型やTimeOfDay型などのデータ型があり、これらを使用することで日付や時刻を自由に操作することができます。

また、TimeLocaleという型を使用することで、日付や時刻の表示形式をカスタマイズすることも可能です。

## 関連情報

- [Data.Timeモジュールのドキュメント](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Data.Timeのチュートリアル](https://www.schoolofhaskell.com/user/commercial/content/a-tutorial-on-package-time)