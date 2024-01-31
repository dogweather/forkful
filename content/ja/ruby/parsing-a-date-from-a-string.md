---
title:                "文字列から日付を解析する"
date:                  2024-01-20T15:38:32.795537-07:00
html_title:           "Arduino: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)

文字列から日付を解析するのは、文字列の形で表された日付をRubyのDateオブジェクトに変換することです。これを行う理由は、日付データを操作したり、形式を変更したりするためが多いです。

## How to: (やり方)

以下は日付を文字列から解析する基本例です。

```ruby
require 'date'

date_string = "2023-04-25"
parsed_date = Date.parse(date_string)

puts parsed_date # => 2023-04-25
```

`Date.parse`メソッドを使うことで簡単に文字列を日付に変換できます。しかし、どのような日付フォーマットも解析しようとするので、フォーマットが不正確な場合はエラーが発生する可能性があります。特定のフォーマットで解析したい場合は、`Date.strptime`を使用します。

```ruby
require 'date'

date_string = "25-04-2023"
format = "%d-%m-%Y"
parsed_date = Date.strptime(date_string, format)

puts parsed_date # => 2023-04-25
```

## Deep Dive (掘り下げ)

RubyのDateライブラリは、多くの日付管理機能を提供しています。`Date.parse`が導入されたのは、開発者が異なる日付表現を一貫したオブジェクトに正規化する必要があったからです。

プログラムにはいくつかの日付解析オプションがあります。例えば、Timeクラスも日付を解析するメソッドを持っていますが、Dateクラスと違って時間も扱います。また、`Date.strptime`メソッドはフォーマットを指定することで解析ができ、これはより厳密な日付入力が求められる場合に便利です。

実装の詳細では、`Date.parse`メソッドは内部で正規表現と日付フォーマットパターンを使用して、さまざまな入力を解析します。しかし、これにはアンビギュアスなケースや解釈の違いが発生し得る点があります。

## See Also (関連する情報)

- [strftime Directive - apidock.com](https://apidock.com/ruby/DateTime/strftime)

これらのリンクは日付と時刻を扱うRubyの様々な側面とメソッドについて詳細な情報を提供します。
