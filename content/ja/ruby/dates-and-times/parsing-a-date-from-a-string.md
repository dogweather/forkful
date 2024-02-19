---
aliases:
- /ja/ruby/parsing-a-date-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:27.341435-07:00
description: "\u2026"
lastmod: 2024-02-18 23:08:55.399781
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？
文字列から日付を解析するとは、日付を表すテキストをRubyが理解できる`Date`または`DateTime`オブジェクトに変換することです。プログラマーは、スケジューリング、分析、またはデータ処理を扱うアプリケーションで一般的な作業である、比較、計算、または日付のフォーマットなどの操作を実行するためにこれを行います。

## 方法:
Rubyでは、標準ライブラリが`Date`および`DateTime`クラスを使用して文字列から日付を解析するための直接的な方法を提供しています。ここでは、Rubyの組み込みメソッドを使用してこれを行う方法を示します：

```ruby
require 'date'

# 文字列から日付を解析する
date_string = "2023-04-01"
parsed_date = Date.parse(date_string)
puts parsed_date
# => 2023-04-01

# より詳細な時間表現のためのDateTime
datetime_string = "2023-04-01T15:30:45+00:00"
parsed_datetime = DateTime.parse(datetime_string)
puts parsed_datetime
# => 2023-04-01T15:30:45+00:00
```

より多くの制御が必要である、または`parse`が直接理解できない形式を処理する場合は、`strptime` (文字列解析時間) を使用し、明示的に形式を指定することができます：

```ruby
# カスタム形式のためのstrptimeを使用
custom_date_string = "01-04-2023"
parsed_date_custom = Date.strptime(custom_date_string, '%d-%m-%Y')
puts parsed_date_custom
# => 2023-04-01
```

### サードパーティのライブラリを使用する:

Rubyの組み込み機能は強力ですが、追加機能やより単純な構文を好む場合には、サードパーティのライブラリを好むことがあります。自然言語解析のための人気の選択肢は`Chronic`gemです：

1. まず、GemfileにChronicを追加し、`bundle install`を実行します：
```ruby
gem 'chronic'
```

2. 次に、以下のように使用します：
```ruby
require 'chronic'

parsed_chronic = Chronic.parse('next Tuesday')
puts parsed_chronic
# 出力は現在の日付に依存します。2023-04-01に解析すると仮定
# => 2023-04-04 12:00:00 +0000
```

`Chronic`は、幅広い自然言語の日付形式を理解できるため、柔軟な日付入力を必要とするアプリケーションにとって強力なツールです。
