---
date: 2024-01-20 17:31:45.094431-07:00
description: "\u65E5\u4ED8\u8A08\u7B97\u306F\u3001\u672A\u6765\u3084\u904E\u53BB\u306E\
  \u7279\u5B9A\u306E\u65E5\u4ED8\u3092\u6C42\u3081\u308B\u3053\u3068\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u4E88\u7D04\u30B7\u30B9\u30C6\u30E0\u3001\
  \u671F\u9593\u8A08\u7B97\u3001\u30ED\u30B0\u89E3\u6790\u306A\u3069\u3067\u4F7F\u3044\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.977466
model: gpt-4-1106-preview
summary: "\u65E5\u4ED8\u8A08\u7B97\u306F\u3001\u672A\u6765\u3084\u904E\u53BB\u306E\
  \u7279\u5B9A\u306E\u65E5\u4ED8\u3092\u6C42\u3081\u308B\u3053\u3068\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u4E88\u7D04\u30B7\u30B9\u30C6\u30E0\u3001\
  \u671F\u9593\u8A08\u7B97\u3001\u30ED\u30B0\u89E3\u6790\u306A\u3069\u3067\u4F7F\u3044\
  \u307E\u3059\u3002"
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付計算は、未来や過去の特定の日付を求めることです。プログラマーは予約システム、期間計算、ログ解析などで使います。

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
