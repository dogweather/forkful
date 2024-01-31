---
title:                "CSVファイルの操作"
date:                  2024-01-19
html_title:           "Arduino: CSVファイルの操作"
simple_title:         "CSVファイルの操作"

category:             "Ruby"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV（Comma-Separated Values）は、データを保存・交換するフォーマットです。RubyでCSVを扱う理由は、データ操作が簡単になり、他のシステムとの連携がスムーズに行えるからです。

## How to:
```Ruby
require 'csv'

# CSVファイルの書き込み
CSV.open('example.csv', 'w') do |csv|
  csv << ["Name", "Age", "City"]
  csv << ["Alice", 20, "Tokyo"]
  csv << ["Bob", 22, "Osaka"]
end

# CSVファイルの読み込み
CSV.foreach('example.csv', headers: true) do |row|
  puts "#{row['Name']}は#{row['Age']}歳で、#{row['City']}に住んでいます。"
end
```

出力例：
```
Aliceは20歳で、Tokyoに住んでいます。
Bobは22歳で、Osakaに住んでいます。
```

## Deep Dive
CSVは1972年にIBMで紹介されました。JSONやXMLといった代替フォーマットもありますが、CSVはシンプルさから多用されます。Rubyの`CSV`ライブラリは、RFC 4180をベースに構築され、柔軟な処理を提供します。`CSV.read`や`CSV.parse`など、さまざまなメソッドが使えます。

## See Also
- [Rubyの公式ドキュメント（CSV）](https://ruby-doc.org/stdlib-2.6.1/libdoc/csv/rdoc/CSV.html)
- [RFC 4180 – CSVの標準](https://tools.ietf.org/html/rfc4180)
