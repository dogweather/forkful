---
title:                "Haskell: 未来や過去の日付の計算"
simple_title:         "未来や過去の日付の計算"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ
将来や過去の日付を計算することに興味がある方のために、この記事ではHaskellを使用して日付の計算を行う方法をご紹介します。

## 方法
以下のコードブロックには、Haskellを使用して日付を計算するためのサンプルコードと出力が示されています。

```Haskell
-- 今日の日付を取得
let today = fromGregorian 2021 9 14

-- 365日後の日付を計算
let futureDate = addDays 365 today

-- 結果を出力
print futureDate
```
出力結果:

```Haskell
2022-09-14
```

## 深堀り
日付を計算する際には、標準ライブラリである`Data.Time.Calendar`モジュールを利用することができ、さまざまな関数を使用して日付の操作が可能です。また、異なるカレンダー形式やタイムゾーンを考慮して日付を計算することもできます。

## は見てみましょう
「さらに読みたい方のために」という意味で、以下のリンクを参考にしてみてください。

[公式Haskellドキュメント：`Data.Time.Calendar`モジュール](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/time-1.9.3/Data-Time-Calendar.html)

[Haskell DateライブラリのGithubページ](https://github.com/glguy/hs-time)