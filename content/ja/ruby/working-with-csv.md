---
title:                "「csvとの作業」"
html_title:           "Ruby: 「csvとの作業」"
simple_title:         "「csvとの作業」"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

「なぜCSVで作業するのか？」
CSVファイルを使うことでデータを簡単に取り扱い、処理することができます。また、データをエクセルやGoogleスプレッドシートなど様々なプログラムで編集することができるため、柔軟性が高く非常に便利です。

「やり方」
```Ruby
require 'csv'

# CSVファイルの読み込み
csv_data = CSV.read('file.csv')

# データの表示
csv_data.each do |row|
  puts row.inspect
end

# データの書き込み
CSV.open('new_file.csv', 'w') do |csv|
  csv << ['Column 1', 'Column 2', 'Column 3']
  csv << ['Data 1', 'Data 2', 'Data 3']
end
```

実行結果:
```
["Column 1", "Column 2", "Column 3"]
["Data 1", "Data 2", "Data 3"]
```

「もっと深く」
CSVファイルを扱う際、いくつかのポイントに注意する必要があります。まず、データにカンマや改行などの区切り文字が含まれている場合は、そのデータをダブルクォーテーションで囲む必要があります。また、データを追加する際は`<<`を使い、CSVファイルからデータを取得する際は`each`メソッドを使うことが一般的です。

「参考資料」
- [RubyでCSVファイルを扱う方法](https://qiita.com/onlyshy/items/06c9105a892b6a6d2f3d)
- [Rubyで読み書きするCSVライブラリの使い方](https://www.techscore.com/blog/2014/07/19/ruby%E3%81%A7%E8%AA%AD%E3%81%BF%E6%9B%B8%E3%81%8D%E3%81%99%E3%82%8Bcsv%E3%83%A9%E3%82%A4%E3%83%96%E3%83%A9%E3%83%AA%E3%82%92%E4%BD%BF%E3%81%84%E6%96%B9/)