---
date: 2024-01-20 17:33:08.659409-07:00
description: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B\u3068\u306F\u30012\u3064\u306E\
  \u65E5\u4ED8\u304C\u540C\u3058\u304B\u3001\u3069\u3061\u3089\u304C\u5148\u304B\u3001\
  \u5F8C\u304B\u3092\u5224\u65AD\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A4\u30D9\u30F3\u30C8\u767A\u751F\u9806\u306E\
  \u7BA1\u7406\u3001\u6709\u52B9\u671F\u9650\u306E\u78BA\u8A8D\u3001\u65E5\u4ED8\u7BC4\
  \u56F2\u5185\u306E\u30C7\u30FC\u30BF\u30D5\u30A3\u30EB\u30BF\u30EA\u30F3\u30B0\u306A\
  \u3069\u3001\u3055\u307E\u3056\u307E\u306A\u76EE\u7684\u3067\u884C\u3044\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.203217-06:00'
model: gpt-4-1106-preview
summary: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B\u3068\u306F\u30012\u3064\u306E\
  \u65E5\u4ED8\u304C\u540C\u3058\u304B\u3001\u3069\u3061\u3089\u304C\u5148\u304B\u3001\
  \u5F8C\u304B\u3092\u5224\u65AD\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A4\u30D9\u30F3\u30C8\u767A\u751F\u9806\u306E\
  \u7BA1\u7406\u3001\u6709\u52B9\u671F\u9650\u306E\u78BA\u8A8D\u3001\u65E5\u4ED8\u7BC4\
  \u56F2\u5185\u306E\u30C7\u30FC\u30BF\u30D5\u30A3\u30EB\u30BF\u30EA\u30F3\u30B0\u306A\
  \u3069\u3001\u3055\u307E\u3056\u307E\u306A\u76EE\u7684\u3067\u884C\u3044\u307E\u3059\
  \u3002."
title: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
weight: 27
---

## What & Why? (何となぜ？)

日付を比較するとは、2つの日付が同じか、どちらが先か、後かを判断することです。プログラマーは、イベント発生順の管理、有効期限の確認、日付範囲内のデータフィルタリングなど、さまざまな目的で行います。

## How to: (やり方)

Haskellで日付を比較する例を見てみましょう。以下は、`Data.Time` モジュールを使った簡単なコードです。

```Haskell
import Data.Time

main :: IO ()
main = do
    let date1 = fromGregorian 2023 3 15 -- 2023年3月15日
    let date2 = fromGregorian 2023 4 1 -- 2023年4月1日
    
    print (date1 == date2) -- False、日付が同じかどうか
    print (date1 < date2) -- True、date1がdate2より前かどうか
    print (date1 > date2) -- False、date1がdate2より後かどうか
```

出力は以下の通りです：
```
False
True
False
```

## Deep Dive (詳細情報)

Haskellでは日付の比較が簡単です。`Data.Time` モジュールがあり、標準で日付操作をサポートしています。Haskellがこれを可能にしているのは、`UTCTime` や `LocalTime` のデータ型が `Ord` クラスのインスタンスであるため、比較演算子が使えるからです。

歴史的に見ると、Haskellは標準的な日付操作を長らく提供してきましたが、初期のライブラリは現在ほど洗練されていませんでした。今日、`Data.Time` は広く使用され、強力な機能を提供しています。

比較の代替方法として、カスタムの比較関数を書くことも可能です。しかし、ほとんどの場合、`Data.Time`の機能で十分です。

実装の詳細としては、`Data.Time` はタイムゾーンをサポートし、日付と時刻の計算も可能です。「ZonedTime」や「TimeZone」のような型を使います。

## See Also (関連情報)

- Haskell `Data.Time` ドキュメント: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html
- Haskell `utctDay` 関数の紹介: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html#g:2
- Haskell Wikiの日付と時刻: https://wiki.haskell.org/Working_with_time

それぞれのリンク先で、より詳細な情報を得ることができます。日付の比較は頻繁に行われるので、`Data.Time` モジュールの機能をよく理解しておくと便利です。
