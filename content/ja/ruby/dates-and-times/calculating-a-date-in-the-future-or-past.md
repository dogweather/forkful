---
date: 2024-01-20 17:31:45.094431-07:00
description: "How to: (\u3084\u308A\u65B9) Ruby\u3067\u672A\u6765\u30FB\u904E\u53BB\
  \u306E\u65E5\u4ED8\u3092\u8A08\u7B97\u3059\u308B\u306B\u306F\u3001`Date` \u30AF\u30E9\
  \u30B9\u3092\u4F7F\u3044\u307E\u3059\u3002\u4EE5\u4E0B\u306B\u7C21\u5358\u306A\u4F8B\
  \u3092\u793A\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.657258-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) Ruby\u3067\u672A\u6765\u30FB\u904E\u53BB\u306E\u65E5\
  \u4ED8\u3092\u8A08\u7B97\u3059\u308B\u306B\u306F\u3001`Date` \u30AF\u30E9\u30B9\u3092\
  \u4F7F\u3044\u307E\u3059\u3002\u4EE5\u4E0B\u306B\u7C21\u5358\u306A\u4F8B\u3092\u793A\
  \u3057\u307E\u3059\u3002"
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
weight: 26
---

## How to: (やり方)
Rubyで未来・過去の日付を計算するには、`Date` クラスを使います。以下に簡単な例を示します。

```Ruby
require 'date'

today = Date.today
# 5日後
future_date = today + 5
# 5日前
past_date = today - 5

puts "今日: #{today}"
puts "5日後: #{future_date}"
puts "5日前: #{past_date}"
```

出力サンプルです:

```
今日: 2023-04-18
5日後: 2023-04-23
5日前: 2023-04-13
```

## Deep Dive (詳細情報)
`Date`クラスはRuby標準ライブラリに含まれているので、追加のgemをインストールする必要はありません。時間も含む計算には`Time`クラスや`DateTime`クラスも使えます。`ActiveSupport`の`advance`メソッドを使うと、より柔軟な日付計算が可能です（Railsでよく使われます）。`DateTime`はマイクロ秒まで考慮するので、細かい時間管理が必要な場合はこちらを選びましょう。

## See Also (関連情報)
- Ruby公式ドキュメント中の `Date` クラス: https://docs.ruby-lang.org/ja/latest/class/Date.html
- `Time` クラスについて: https://docs.ruby-lang.org/ja/latest/class/Time.html
- `ActiveSupport`の拡張メソッド: https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html
- Rubyでの時刻処理に関するガイド: https://www.rubyguides.com/2015/12/ruby-time/
