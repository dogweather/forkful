---
title:                "文字列から日付を解析する"
html_title:           "Bash: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
文字列から日付を解析するとは、日付情報を含む文字列データを操作可能な日付形式に変換することです。なぜなら、これによりプログラマーは日付に関する様々な操作（比較、計算など）を簡単に実行できるからです。

## 手順:
Rubyでは、`Date.parse`メソッドを使用して文字列から日付を解析できます。

```ruby
require 'date'

string_date = "2022-04-01"
date = Date.parse(string_date)
puts date
```
出力結果は次のとおりです。

```
2022-04-01
```

## 詳細分析:
歴史的な背景: テキスト形式で日付を保存することは古くから一般的に行われて来ましたが、それをプログラムで効率的に利用するためには解析が不可欠です。

代替手段: `strptime`メソッドもあります。それはより具体的な日付形式を解析します。

```ruby
require 'date'

string_date = "01-04-2022"
date = Date.strptime(string_date, "%d-%m-%Y")
puts date
```
実装詳細: `Date.parse`メソッドは内部的に`Date._parse`を使用して文字列を解析し、その結果を`Date.new`に渡してDateオブジェクトを作成します。

## 参考リンク:
1. [Ruby Date Documentation](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html)
2. [Date Parsing in Ruby](https://www.rubyguides.com/2015/12/ruby-time/)