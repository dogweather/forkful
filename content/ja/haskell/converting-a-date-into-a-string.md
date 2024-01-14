---
title:    "Haskell: 日付を文字列に変換する"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換するのは、プログラマーがデータをより読みやすく、効率的に処理するために役立ちます。例えば、データベースから取得した日付をユーザーに表示する場合、文字列に変換することで読みやすくなります。

## 方法

Haskellでは、date型を文字列に変換するために`formatTime`関数を使います。以下のようなコードを書くことで、日付を特定のフォーマットで表示することができます。

```Haskell
import Data.Time.Format
import Data.Time.LocalTime

timestamp :: LocalTime -> String
timestamp date = formatTime defaultTimeLocale "%Y年%m月%d日 %H時%M分%S秒" date

main :: IO ()
main = do
    let date = LocalTime { localDay = fromGregorian 2021 10 25
                         , localTimeOfDay = TimeOfDay 14 30 0
                         }
    putStrLn $ timestamp date
```
上記の例では、2021年10月25日 14時30分00秒というフォーマットで日付を表示しています。`defaultTimeLocale`を変えることで、異なるロケールの日付表記を行うこともできます。

## ディープダイブ

Haskellには日付型として、`UTCTime`や`ZonedTime`などがあります。これらの型は、タイムゾーンや夏時間の情報を持っているため、正しい日付を取得することができます。

また、`Data.Time.Format`モジュール内には、さまざまな関数があります。例えば、`parseTimeM`関数を使うことで、文字列を日付型に変換することができます。

## 参考リンク

- [Haskellで今日の日付を取得する方法](https://qiita.com/kuroeqs/items/070eb3a6c419c76efa4a)
- [Haskell Data.Timeモジュールの使い方](http://saito-tsutomu.hatenablog.com/entry/20120406/1333704261)
- [Hackage: Data.Time.Format](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html)

## 関連リンク

- [Haskellの基礎を学ぶ入門記事一覧](https://qiita.com/tags/haskell)
- [日付操作のための便利なHaskellライブラリ一覧](https://qiita.com/criztiani/items/15625a57c790f691f4ad)