---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 日付を文字列に変換する：Ruby編

## 何となぜ？

日付を文字列に変換するとは、日付データを人間が理解しやすいテキスト形式で表示することです。これはデータの読みやすさを高めたり、異なる日付形式間での互換性を実現したりするためにプログラマーがよく行います。

## 方法：

Rubyでは、date 库を使用して日付を文字列に変換します。

```Ruby
require 'date'

d = Date.today
puts d.to_s
```
出力：

```Ruby
"2022-02-15"
```

また、strftimeメソッドを使って日付フォーマットをカスタマイズすることもできます。

```Ruby
puts d.strftime("%d/%m/%Y")
```
出力：

```Ruby
"15/02/2022"
```

## 詳細:

Rubyのstrftimeメソッドは過去にCで導入され、あらゆる種類の日付と時間の形式を生成するための枠組みを提供します。

同種の方法として、RailsのI18n.lメソッドがあります。これは特定のロケールで日付を文字列に変換します。

実装の詳細では、Date#to_s メソッドは内部的に ISO 8601 形式（yyyy-mm-dd）で日付を出力します。一方strftimeメソッドでは、文字列フォーマットの指示子を使用して出力形式を制御できます。

## 参照：

以下のリンクには、日付と文字列の変換に関するより深い情報があります：

- Ruby Docs: [Date](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html)
- Stack Overflow: [How to convert a date to a string in Ruby](https://stackoverflow.com/questions/5410682/how-to-convert-a-date-to-a-string-in-ruby)
- Ruby Guides: [Ruby strftime method](https://www.rubyguides.com/2015/12/ruby-time/)