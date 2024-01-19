---
title:                "現在の日付を取得する"
html_title:           "PowerShell: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

##何を、なぜ？
現在の日付を取得するとは、簡単に言うと今日が何日かを知ることです。プログラマーは、アプリケーションが日付や時刻に基づいた機能を提供するため、またはログ記録を放り込むためにこの操作を行います。

##やり方:
Rubyでは、現在の日付や時刻を取得することはとても簡単です。基本的なコードは以下のようになります：

```Ruby
require 'date'

current_date = Date.today
puts current_date
```

実行すると、次のような出力が得られます（実行日によります）：

```
2022-03-10
```

##深層解析:
RubyのDateクラスは1970年に引き続き開発され、Rubyの一部として初めてリリースされました。Date.todayメソッドは、システムの現在の日付を返します。

また、現在の日時を取得する別の方法として、Time.nowメソッドもありますが、これは時間まで取得できます。以下は具体的なコードです：

```Ruby
current_time = Time.now
puts current_time
```

実行すると下記のような出力が得られます：

```
2022-03-10 12:34:56 +0900
```

この方法はDate.todayより詳細な情報を取得できますが、日付だけが必要な場合はDate.todayの方がシンプルです。

##参考のために：
以下に、本記事と関連するリンクを示します。

1. Ruby公式ドキュメンテーション：Date - https://docs.ruby-lang.org/ja/latest/class/Date.html
2. Ruby公式ドキュメンテーション：Time - https://docs.ruby-lang.org/ja/latest/class/Time.html