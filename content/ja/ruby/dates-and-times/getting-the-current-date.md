---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:40.768202-07:00
description: "\u65B9\u6CD5\uFF1A Ruby\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u306B\u306F\u3001\u65E5\u4ED8\u3068\u6642\u9593\u3092\u6271\u3046\u305F\u3081\u306E\
  `Date`\u30AF\u30E9\u30B9\u3068`Time`\u30AF\u30E9\u30B9\u304C\u542B\u307E\u308C\u3066\
  \u3044\u307E\u3059\u3002\u4EE5\u4E0B\u304C\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\
  \u5F97\u3059\u308B\u65B9\u6CD5\u3067\u3059\uFF1A."
lastmod: '2024-04-05T21:53:43.653978-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

## 方法：
Rubyの標準ライブラリには、日付と時間を扱うための`Date`クラスと`Time`クラスが含まれています。以下が現在の日付を取得する方法です：

```ruby
require 'date'

current_date = Date.today
puts current_date
```

サンプル出力：
```
2023-04-12
```

日付に時刻を含める場合、Rubyの`Time`クラスが適しています：

```ruby
current_time = Time.now
puts current_time
```

サンプル出力：
```
2023-04-12 14:33:07 +0200
```

タイムゾーンの管理など、より多くの機能が必要な場合は、`ActiveSupport`のようなサードパーティ製のgemを使用することが望ましいかもしれません（Railsの一部ですが、単独で使用できます）。

まず、Gemfileに`activesupport`を追加し、`bundle install`を実行します：

```ruby
gem 'activesupport'
```

そして、タイムゾーンを扱うには：

```ruby
require 'active_support/time'

Time.zone = 'Eastern Time (US & Canada)'  # 希望のタイムゾーンを設定
current_time_with_zone = Time.zone.now
puts current_time_with_zone
```

サンプル出力：
```
Wed, 12 Apr 2023 08:33:07 EDT -04:00
```
