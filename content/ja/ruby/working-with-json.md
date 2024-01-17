---
title:                "「Jsonを使う」"
html_title:           "Ruby: 「Jsonを使う」"
simple_title:         "「Jsonを使う」"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

JSONとは、データのやり取りに使われるフォーマットの一つです。プログラマーがJSONを使う理由は、データをより簡単かつ効率的に処理できるからです。

## 方法：

```ruby
require 'json'

# JSONデータをパースする
data = JSON.parse('{"name": "John", "age": 30}')

# ハッシュとしてデータを取得する
puts data["name"]
# => "John"

# ハッシュからJSONデータを生成する
json_data = data.to_json
puts json_data
# => '{"name": "John", "age": 30}'
```

## 深堀り：

1. JSONはJavaScript Object Notationの略称であり、JavaScriptでデータを扱うために作られました。
2. JSONにはXMLやCSVなどの他のフォーマットと比べて、よりシンプルで扱いやすい特徴があります。
3. JSONを使う際は、正しいフォーマットに従うことが重要です。

## 関連情報：

- [RubyのJSONライブラリ公式ドキュメント](https://docs.ruby-lang.org/en/master/JSON.html)
- [RubyGemsで利用可能なJSONパースライブラリ一覧](https://rubygems.org/search?utf8=%E2%9C%93&query=json)
- [JSONフォーマットの詳細について](https://www.json.org/json-ja.html)