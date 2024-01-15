---
title:                "「未来や過去の日付の計算」"
html_title:           "Haskell: 「未来や過去の日付の計算」"
simple_title:         "「未来や過去の日付の計算」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

あなたは日付を未来や過去に計算する必要がありますか？例えば、誕生日や締め切りの日付を知りたい場合や、プログラムのロジックで日付を操作する必要がある場合など、日常生活や開発プロジェクトで様々なシーンで利用されます。この記事では、Haskellを使用して未来や過去の日付を計算する方法を紹介します。

## 方法

```Haskell
-- 今日の日付を取得
today :: Day
today = utctDay <$> getCurrentTime

-- 未来の日付を計算する関数
futureDate :: Integer -> Day
futureDate days = addDays days <$> today

-- 過去の日付を計算する関数
pastDate :: Integer -> Day
pastDate days = addDays (days * (-1)) <$> today

-- 未来の日付を計算し、結果を出力
main = do
  let birthday = futureDate 365 -- 未来の誕生日を1年後に計算
  print $ "Your birthday next year will be on " ++ show birthday

  let projectDeadline = futureDate 7 -- 未来のプロジェクト締め切り日を1週間後に計算
  print $ "The project deadline is " ++ show projectDeadline ++ "! Get ready!"

  let tenDaysAgo = pastDate 10 -- 過去の日付を10日前に計算
  print $ "10 days ago was " ++ show tenDaysAgo ++ ". Time flies!"
```

## ディープダイブ

日付を扱う際には、Haskellのdateパッケージを使用することが推奨されます。dateパッケージには、日付や時刻を操作するための便利な関数が多数用意されています。また、単位やタイムゾーンの変換を行う機能もあり、国際的な開発にも役立ちます。

## See Also

- [Hackage - dateパッケージ](https://hackage.haskell.org/package/date)
- [Haskell Wiki - 日付や時刻の操作](https://wiki.haskell.org/Date_and_time)
- [Qiita - Haskellで日付や時刻を扱う方法](https://qiita.com/yudai-nkt/items/1ade98f82ea50fdf3582)