---
title:    "Haskell: 将来または過去の日付の計算"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

日付の計算をするのには理由があります。例えば、将来の特定の日付の予定を知りたい、または過去の特定の日付を追跡したい場合があります。

## How To

Haskellを使用して日付の計算をする方法は簡単です。```Haskell
import Data.Time
import Data.Time.Calendar
-- 明日の日付を計算する
tomorrow :: IO ()
tomorrow = do
  today <- utctDay <$> getCurrentTime
  let nextDay = addDays 1 today
  putStrLn $ "Tomorrow's date is: " ++ show nextDay
```
出力はコンソールに```Tomorrow's date is: 2021-10-13```のように表示されます。

## Deep Dive

日付の計算には、まずData.Timeモジュールを使用してタイムゾーンを設定する必要があります。その後、特定の日付を表すData.Time.Calendarを使用して、日付に関する情報を取得する必要があります。addDays関数を使用することで、与えられた日数分だけ日付を加算または減算することができます。さらに、カレンダーの種類や時間帯などのパラメータを設定することもできます。

## See Also

* [Data.Timeモジュールのドキュメント](https://hackage.haskell.org/package/time/docs/Data-Time.html)
* [Data.Time.Calendarモジュールのドキュメント](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html)
* [Haskellにおける日付の計算の応用例](https://qiita.com/nirasan/items/7425caff75750a0be048)