---
title:                "「csvを使用する」"
html_title:           "Ruby: 「csvを使用する」"
simple_title:         "「csvを使用する」"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## 何それ？

CSVとは、Comma Separated Valuesの略称で、コンマで区切られたテキストデータを扱うためのファイルフォーマットのことです。プログラマーは、データベースやスプレッドシートなど複数のソースから取得したデータを集約し、処理するためによく使用します。

## 方法：

Rubyでは、CSVファイルを読み込み、書き込むためにCSVライブラリが使用できます。下記のコードは、CSVファイルを読み込んでデータを表示する例です。

```Ruby
require 'csv'

CSV.foreach('data.csv') do |row|
  puts row.join(', ')
end
```

出力：

```Ruby
Alice, 20
Bob, 30
```

## 深く掘り下げる

CSVは、コンマの他にも、タブやセミコロンなどを区切り文字として使用することができます。また、CSVファイルにはヘッダー行を含め、列のタイトルを表示することもできます。

Ruby以外にも、PythonやJavaなどの他のプログラミング言語でもCSVファイルを扱うことができます。また、データベースやスプレッドシートから直接CSV形式でデータをエクスポートすることもできます。

CSVファイルを扱う際には、データの入力が正しく行われているかを確認するため、バリデーションを行うことが重要です。

## 関連リンク

- [CSVライブラリドキュメント](https://ruby-doc.org/stdlib-2.6.3/libdoc/csv/rdoc/CSV.html)
- [PythonのCSVモジュール](https://docs.python.org/3/library/csv.html)
- [JavaのOpenCSVライブラリ](http://opencsv.sourceforge.net/)