---
title:                "現在の日付の取得"
aliases: - /ja/ruby/getting-the-current-date.md
date:                  2024-02-03T19:10:40.768202-07:00
model:                 gpt-4-0125-preview
simple_title:         "現在の日付の取得"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
