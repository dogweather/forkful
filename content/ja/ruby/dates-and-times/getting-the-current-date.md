---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:40.768202-07:00
description: "\u307B\u3068\u3093\u3069\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\
  \u306E\u53D6\u308A\u7D44\u307F\u306B\u304A\u3044\u3066\u3001\u30A2\u30D7\u30EA\u30B1\
  \u30FC\u30B7\u30E7\u30F3\u3067\u306E\u6D3B\u52D5\u306E\u30ED\u30B0\u53D6\u308A\u304B\
  \u3089\u3001\u65E5\u4ED8\u5370\u306E\u3042\u308B\u30EC\u30DD\u30FC\u30C8\u306E\u751F\
  \u6210\u307E\u3067\u3001\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\
  \u3053\u3068\u306F\u57FA\u672C\u7684\u306A\u30BF\u30B9\u30AF\u3067\u3059\u3002Ruby\u3067\
  \u306F\u3001\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3057\u3066\
  \u3053\u308C\u3092\u5BB9\u6613\u306B\u5B9F\u884C\u3067\u304D\u3001\u65E5\u4ED8\u306B\
  \u95A2\u308F\u308B\u64CD\u4F5C\u3092\u30B7\u30F3\u30D7\u30EB\u306B\u3057\u307E\u3059\
  \u3002"
lastmod: '2024-03-13T22:44:42.868946-06:00'
model: gpt-4-0125-preview
summary: "\u307B\u3068\u3093\u3069\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\
  \u306E\u53D6\u308A\u7D44\u307F\u306B\u304A\u3044\u3066\u3001\u30A2\u30D7\u30EA\u30B1\
  \u30FC\u30B7\u30E7\u30F3\u3067\u306E\u6D3B\u52D5\u306E\u30ED\u30B0\u53D6\u308A\u304B\
  \u3089\u3001\u65E5\u4ED8\u5370\u306E\u3042\u308B\u30EC\u30DD\u30FC\u30C8\u306E\u751F\
  \u6210\u307E\u3067\u3001\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\
  \u3053\u3068\u306F\u57FA\u672C\u7684\u306A\u30BF\u30B9\u30AF\u3067\u3059\u3002Ruby\u3067\
  \u306F\u3001\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3057\u3066\
  \u3053\u308C\u3092\u5BB9\u6613\u306B\u5B9F\u884C\u3067\u304D\u3001\u65E5\u4ED8\u306B\
  \u95A2\u308F\u308B\u64CD\u4F5C\u3092\u30B7\u30F3\u30D7\u30EB\u306B\u3057\u307E\u3059\
  \u3002"
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
---

{{< edit_this_page >}}

## 何となぜ？
ほとんどのプログラミングの取り組みにおいて、アプリケーションでの活動のログ取りから、日付印のあるレポートの生成まで、現在の日付を取得することは基本的なタスクです。Rubyでは、標準ライブラリを使用してこれを容易に実行でき、日付に関わる操作をシンプルにします。

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
