---
date: 2024-01-20 17:37:22.328019-07:00
description: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u3068\
  \u306F\u3001`Date` \u3084 `Time` \u306E\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\
  \u8AAD\u307F\u3084\u3059\u3044\u30C6\u30AD\u30B9\u30C8\u5F62\u5F0F\u306B\u3059\u308B\
  \u3053\u3068\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30ED\u30B0\u8A18\
  \u9332\u3001\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30A4\u30B9\
  \u3001\u30C7\u30FC\u30BF\u306E\u4FDD\u5B58\u3084\u4EA4\u63DB\u306E\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.870532-06:00'
model: gpt-4-1106-preview
summary: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u3068\
  \u306F\u3001`Date` \u3084 `Time` \u306E\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\
  \u8AAD\u307F\u3084\u3059\u3044\u30C6\u30AD\u30B9\u30C8\u5F62\u5F0F\u306B\u3059\u308B\
  \u3053\u3068\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30ED\u30B0\u8A18\
  \u9332\u3001\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30A4\u30B9\
  \u3001\u30C7\u30FC\u30BF\u306E\u4FDD\u5B58\u3084\u4EA4\u63DB\u306E\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3046\u3002."
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

## How to: (方法：)
```ruby
require 'date'

# 現在の日付と時刻の取得
current_time = Time.now
# 日付を文字列に変換
date_string = current_time.strftime("%Y-%m-%d %H:%M:%S")

puts date_string
# 出力例: "2023-04-06 15:23:47"
```

`strftime` メソッドを使って、さまざまなフォーマットで日付を文字列に変換することもできます。

```ruby
# 別のフォーマット例
friendly_date_string = current_time.strftime("%A, %B %d, %Y")
puts friendly_date_string
# 出力例: "Thursday, April 06, 2023"
```

## Deep Dive (深掘り：)
Ruby での日付の文字列変換は、標準ライブラリの `Date` と `Time` クラスで簡単に扱えます。この機能は、時刻データの人間による読解やデータベースの操作、JavaScript との連携など、多岐に渡る処理の中心になっています。
歴史的な文脈では、Rubyの `strftime` メソッドは、UNIXの `strftime()` 関数から影響を受けています。これは、豊富なフォーマットオプションを提供し、柔軟な日付文字列を生成することができます。
代替手段として、`to_s` メソッドや `String` クラスの `Date#iso8601` メソッドなどもあります。これらは、特定の形式で迅速に日付を文字列に変換するときに役立ちます。

## See Also (関連リンク：)
- Rubyの公式ドキュメントの `Time` クラス: [https://ruby-doc.org/core-2.7.0/Time.html](https://ruby-doc.org/core-2.7.0/Time.html)
- Rubyの公式ドキュメントの `Date` クラス: [https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- `strftime` のフォーマット指定子の詳細: [https://apidock.com/ruby/DateTime/strftime](https://apidock.com/ruby/DateTime/strftime)
