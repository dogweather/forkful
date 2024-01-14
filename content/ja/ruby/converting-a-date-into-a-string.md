---
title:                "Ruby: 「日付を文字列に変換する」"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

日付を文字列に変換することは、日付情報をより見やすく表示したり、データベースに保存したりするために重要なことです。 Rubyプログラムで日付を扱う際には、よく使われる機能の一つです。

## How To

```Ruby
# 今日の日付を取得
today = Date.today

# 日付を文字列に変換（デフォルトフォーマット）
puts today.to_s
=> "2020-10-08"

# 日付を文字列に変換（指定したフォーマット）
puts today.strftime("%Y年%m月%d日")
=> "2020年10月08日"
```

## Deep Dive

日付を文字列に変換する際には、strftimeメソッドを使用します。このメソッドは、日付を指定したフォーマットに従って文字列に変換してくれます。フォーマットに使用できる記号はたくさんありますが、よく使われるものは次のようなものです。

- %Y: 年（4桁）
- %m: 月（2桁）
- %d: 日（2桁）
- %H: 時間（24時間制、2桁）
- %M: 分（2桁）
- %S: 秒（2桁）
- %p: AM/PM
- %a: 曜日の省略形
- %A: 曜日の全称

日付を文字列に変換する際には、必要に応じてこれらの記号を組み合わせてフォーマットを作成してください。詳細な記号の一覧や使用例は、公式ドキュメントを参照すると良いでしょう。

## See Also

- Date and Time class documentation: https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html
- strftime method documentation: https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html#method-i-strftime
- strftime formatting guide: https://apidock.com/ruby/DateTime/strftime