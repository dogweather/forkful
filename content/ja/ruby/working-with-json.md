---
title:                "JSONを使ったプログラミング"
html_title:           "Ruby: JSONを使ったプログラミング"
simple_title:         "JSONを使ったプログラミング"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/working-with-json.md"
---

{{< edit_this_page >}}

##なぜ

JSONを使って作業することのメリットは、データをやりとりする際に必要となるフォーマットが一貫していることです。データの整理や解析を簡単にするために、JSONはとても便利なツールとなります。

##使い方

```Ruby
#JSONをパースする
require 'json'

json_data = '{"name": "John", "age": 25, "profession": "developer"}'

user_info = JSON.parse(json_data) #JSONデータをHashに変換

puts user_info["name"] #結果 - John
puts user_info["age"] #結果 - 25
puts user_info["profession"] #結果 - developer
```

```Ruby
#JSONを作成する
require 'json'

user_info = {
  "name": "Jane",
  "age": 30,
  "profession": "designer"
}

json_data = JSON.generate(user_info) #HashをJSONに変換

puts json_data #結果 - {"name": "Jane", "age": 30, "profession": "designer"}
```

##深堀り

JSONは、Webサイトやアプリケーションでよく使用されるデータ形式です。Hashと似ていますが、より軽量でデータのやりとりに適したフォーマットです。Rubyでは、標準ライブラリとしてJSONモジュールが提供されており、手軽にJSONを扱うことができます。

##参考文献

- [Ruby公式ドキュメント - JSON](https://docs.ruby-lang.org/ja/latest/class/JSON.html)
- [JSONとは？メリットや使い方を解説！](https://tech-boost.jp/common/column/71/)