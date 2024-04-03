---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:27.341435-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.867498-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\
  \u3068\u306F\u3001\u65E5\u4ED8\u3092\u8868\u3059\u30C6\u30AD\u30B9\u30C8\u3092Ruby\u304C\
  \u7406\u89E3\u3067\u304D\u308B`Date`\u307E\u305F\u306F`DateTime`\u30AA\u30D6\u30B8\
  \u30A7\u30AF\u30C8\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\
  \u30B0\u3001\u5206\u6790\u3001\u307E\u305F\u306F\u30C7\u30FC\u30BF\u51E6\u7406\u3092\
  \u6271\u3046\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3067\u4E00\u822C\u7684\
  \u306A\u4F5C\u696D\u3067\u3042\u308B\u3001\u6BD4\u8F03\u3001\u8A08\u7B97\u3001\u307E\
  \u305F\u306F\u65E5\u4ED8\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u306A\u3069\u306E\
  \u64CD\u4F5C\u3092\u5B9F\u884C\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002."
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
weight: 30
---

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
