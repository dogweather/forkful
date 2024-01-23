---
title:                "文字列から日付を解析する"
date:                  2024-01-20T15:36:58.951392-07:00
html_title:           "Arduino: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Haskellでの日付文字列の解析

## 何となぜ？ (What & Why?)
日付文字列を解析するとは、テキスト形式で書かれた日付をプログラムが理解できる形式に変換する行為です。データ入力、ログ分析、ユーザーインターフェースなど、様々な場面で必要とされます。

## どうやって？ (How to:)
以下は`time`ライブラリを使った日付文字列の解析の例です。ソースコード内のコメントも確認してください。

```Haskell
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.Clock (UTCTime)

-- 日付文字列を解析し、Maybe UTCTime を返す関数
parseDate :: String -> Maybe UTCTime
parseDate = parseTimeM defaultTimeLocale "%Y-%m-%d %H:%M:%S"

main :: IO ()
main = do
  let exampleDateString = "2023-03-14 15:09:26"
  print $ parseDate exampleDateString  -- 結果のサンプル出力: Just 2023-03-14 15:09:26 UTC
```

このコードは "2023-03-14 15:09:26" のような日付文字列を `UTCTime` 型に変換します。

## 詳細解説 (Deep Dive)
`time`ライブラリの `parseTimeM` 関数は、Haskellにおける日付と時刻の解析の標準的な手段です。`%Y-%m-%d %H:%M:%S` という書式指定子は、ISO 8601 形式の日付と時刻を解析するのに用います。
過去には `old-time` ライブラリがよく使われましたが、今日では `time` が広く推奨されています。別の選択肢として、より強力なパーサーを持つ `Data.Time.Calendar` や `Data.Time.Clock` が存在します。
解析処理の内部実装は、パーサーコンビネータを用いて文字列から日付データへと段階的に変換しています。

## 関連情報 (See Also)
- Haskell `time` ライブラリドキュメント: https://hackage.haskell.org/package/time
- `Data.Time.Format` の書式指定子: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html#v:formatTime
- ISO 8601 標準: https://www.iso.org/iso-8601-date-and-time-format.html
