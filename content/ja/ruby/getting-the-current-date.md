---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:16:13.668866-07:00
html_title:           "Bash: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
プログラマが現在の日付を取得することは、データにタイムスタンプを付けたり、特定の日付に基づいた処理を行うためです。シンプルですが、多くのアプリケーションには不可欠です。

## How to: (方法)
```Ruby
require 'date'

# 現在の日付を取得する
current_date = Date.today
puts current_date
```
サンプル出力:
```
2023-04-15
```

## Deep Dive (掘り下げ)
Rubyでは`Date`クラスを使って日付を扱います。`Date.today`で今日の日付を取得できます。これは単純ですが、Rubyの歴史的背景では、`Time`クラスやライブラリを利用することも多かったです。`DateTime`や`ActiveSupport`のようなライブラリ提供のメソッドも使われることがあります。ただし、必要とする機能が`Date`クラスで完結するなら、組み込みのクラスを利用する方がシンプルです。

Rubyが成熟してくると、多くの開発者が`Time`より`Date`を選ぶようになりました。それは、日付だけを扱う場合には`Time`クラスのインスタンスは多くの情報を持ちすぎており、メモリ効率が良くないためです。

しかし、時刻も扱いたい場合、`DateTime`を使うことが勧められます。現在の日付と時刻を取得するには、以下のようにします。

```Ruby
current_datetime = DateTime.now
puts current_datetime
```

サンプル出力:
```
2023-04-15T14:33:20+09:00
```

このように、Rubyでは現在の日付や時刻を取得する方法が幾つもありますが、状況に応じて最も適切なものを選びましょう。

## See Also (関連情報)
- RubyのTimeクラスのドキュメント：[Ruby-Doc Time](https://ruby-doc.org/core/Time.html)
- ActiveSupportの時間拡張について：[API Dock ActiveSupport::Time](https://apidock.com/rails/ActiveSupport/Time)
