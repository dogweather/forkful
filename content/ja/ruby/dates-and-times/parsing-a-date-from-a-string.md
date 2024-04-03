---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:27.341435-07:00
description: "\u65B9\u6CD5: Ruby\u3067\u306F\u3001\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u304C`Date`\u304A\u3088\u3073`DateTime`\u30AF\u30E9\u30B9\u3092\u4F7F\u7528\
  \u3057\u3066\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\
  \u305F\u3081\u306E\u76F4\u63A5\u7684\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3066\
  \u3044\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001Ruby\u306E\u7D44\u307F\u8FBC\
  \u307F\u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u7528\u3057\u3066\u3053\u308C\u3092\u884C\
  \u3046\u65B9\u6CD5\u3092\u793A\u3057\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.867498-06:00'
model: gpt-4-0125-preview
summary: "Ruby\u3067\u306F\u3001\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u304C`Date`\u304A\
  \u3088\u3073`DateTime`\u30AF\u30E9\u30B9\u3092\u4F7F\u7528\u3057\u3066\u6587\u5B57\
  \u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\u305F\u3081\u306E\u76F4\
  \u63A5\u7684\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\
  \u3053\u3053\u3067\u306F\u3001Ruby\u306E\u7D44\u307F\u8FBC\u307F\u30E1\u30BD\u30C3\
  \u30C9\u3092\u4F7F\u7528\u3057\u3066\u3053\u308C\u3092\u884C\u3046\u65B9\u6CD5\u3092\
  \u793A\u3057\u307E\u3059\uFF1A."
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
