---
title:                "Haskell: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換する理由は何でしょうか。日付を文字列に変換することによって、日付をより読みやすく表示したり、データベースに保存したりすることができるからです。

## 方法

Haskellで日付を文字列に変換するには、`Data.Time.Format`モジュールの`formatTime`関数を使用します。以下のコードは、現在の日付を"年-月-日"の形式の文字列に変換する例です。

```Haskell
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (getCurrentTime)
import Text.Printf (printf)

main = do
  currentTime <- getCurrentTime
  let dateString = formatTime defaultTimeLocale "%Y-%m-%d" currentTime
  printf "今日の日付は %s です。" dateString
```

上記のコードを実行すると、以下のような出力が得られます。

```
今日の日付は 2020-02-18 です。
```

日付のフォーマットは`%Y`が年、`%m`が月、`%d`が日を表すことに注意してください。詳細なフォーマットの書き方は、[Haskellドキュメント](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html#v:formatTime)を参照してください。

## 深堀り

日付を文字列に変換する際には、コンピュータ内部での日付の表現形式や、ロケール（地域や言語によって異なる日付の表記方法）についても考慮する必要があります。また、`Data.Time.Format`モジュール以外にも、`Data.Time.LocalTime`や`Data.Time.Calendar`などのモジュールを使用することで、より詳細な日付操作が可能です。

## 関連リンク

- [Haskellドキュメント：日付を文字列に変換する方法](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html#v:formatTime)
- [日本語版Haskell wiki：日付と時刻を扱う](https://wiki.haskell.org/%E6%97%A5%E4%BB%98%E3%81%A8%E6%99%82%E5%88%BB%E3%82%92%E6%89%B1%E3%81%86)
- [Haskell関数型プログラミング：日付と時刻の操作](https://techbookfest.org/product/6256654045328384?productVariantID=5498067650775040)