---
title:                "将来または過去の日付を計算する"
date:                  2024-01-20T17:31:45.094431-07:00
model:                 gpt-4-1106-preview
simple_title:         "将来または過去の日付を計算する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/calculating-a-date-in-the-future-or-past.md"
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
