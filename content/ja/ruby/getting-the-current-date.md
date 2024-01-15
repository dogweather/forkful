---
title:                "現在の日付を取得する"
html_title:           "Ruby: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
なぜ、今日の日付を取得するのか？
Rubyプログラミングでは、現在の日付を取得することは非常に一般的なタスクです。特定の日付を記録したり、プログラムを日付に基づいて動作させる必要があるため、日付を取得することは非常に重要です。

## How To
まず、RubyのDateライブラリを使用する必要があります。以下の例を参考にしてください。

```Ruby
require 'date'

# 今日の日付を取得する
today = Date.today
puts "今日の日付は#{today}です"

# 明日の日付を取得する
tomorrow = Date.today + 1
puts "明日の日付は#{tomorrow}です"

# 特定の日付から曜日を取得する
my_birthday = Date.parse("1990-10-01")
puts "私の誕生日は#{my_birthday.strftime("%A")}です"
```

以下のような出力が得られます。

```
今日の日付は2020-07-25です
明日の日付は2020-07-26です
私の誕生日はMondayです
```

## Deep Dive
さらに深くRubyで日付を取得する方法を探求しましょう。日付を取得するために主に使用されるメソッドは以下の2つです。

- `Date.today`：現在の日付を取得します。
- `Date.parse("yyyy-mm-dd")`：指定した日付のオブジェクトを作成します。

また、取得した日付オブジェクトには様々なメソッドがあります。例えば、`strftime`メソッドを使用することで、特定のフォーマットで日付を出力することもできます。

詳細は公式ドキュメントを参照してください。

## See Also
関連リンク：

- [Rubyの公式ドキュメント](https://www.ruby-lang.org/ja/documentation/)
- [Dateライブラリのソースコード](https://github.com/ruby/date)

以上で日付を取得する方法を学びました。RubyのDateライブラリを使うことで、日付を簡単に取得することができます。ぜひ活用してみてください。