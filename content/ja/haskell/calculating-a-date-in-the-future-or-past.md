---
title:                "将来または過去の日付の計算"
html_title:           "Haskell: 将来または過去の日付の計算"
simple_title:         "将来または過去の日付の計算"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何となぜ？

日付の計算は、現在から一定の日数後または前の日付を求めるプロセスです。プログラマーは、イベントのスケジュール設定や料金計算などのために日付の計算を行います。

## 方法:

日付計算には、`Data.Time`モジュールの`addDays`関数を使用します。記述するコードは以下の通りです：

```Haskell
import Data.Time

calculateFutureDate :: Integer -> IO ()
calculateFutureDate days = do
  currentDate <- getCurrentTime
  let futureDate = addUTCTime (days * 24 * 60 * 60) currentDate
  print futureDate
```
これを実行すると、結果は以下のようになります。

```Haskell
> calculateFutureDate 5
2022-06-02 08:46:10.6759461 UTC
```

このコードでは、`addUTCTime`関数を使用して現在の日付に特定の秒数を追加しています。これにより未来の日付が計算されます。

## ディープダイブ：

短い歴史的背景: Haskellの`Data.Time`モジュールは2006年にAshley Yakeleyによって作成されました。以来、このモジュールはHaskellの日付と時刻の計算の主要な手段となっています。

代替手段: Haskell以外の言語では、特定の日数を追加または引くために様々な手法が使われています。例えば、Pythonでは`datetime.timedelta`、Javaでは`java.time.Period`などがあります。

実装詳細: Haskellの`Data.Time`モジュールでは、関数`addUTCTime`は`DiffTime`（秒）を受け取り、その秒数を追加した新しい`UTCTime`を生成します。この秒数は常に24時間制を使用し、1日を86400秒として扱います。

## 関連情報：

- Haskellの日付と時間に関する詳細なドキュメント: [Data.Time モジュールドキュメント](http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- PythonやJavaでの同様な操作の情報: [Python: datetime Module](https://docs.python.org/3/library/datetime.html#timedelta-objects), [Java: java.time.Period](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/Period.html)