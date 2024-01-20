---
title:                "JSONを扱う方法"
html_title:           "Arduino: JSONを扱う方法"
simple_title:         "JSONを扱う方法"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (なにを、なぜする？)
JSON （JavaScript Object Notation）はデータ形式。プログラマは情報交換にJSONを使う。読みやすく、構造化されたデータをWebサーバやAPI間でやり取りするため。

## How to: (使い方)
RubyでJSONを使うには `json` ライブラリが必要。以下は基本例。

```Ruby
require 'json'

# JSON文字列をRubyのハッシュに変換
json_string = '{"name": "Taro", "age": 30, "city": "Tokyo"}'
person = JSON.parse(json_string)
puts person["name"]  # 出力: Taro

# RubyのハッシュをJSON文字列に変換
person = { name: "Hanako", age: 25, city: "Kyoto" }
json_output = person.to_json
puts json_output  # 出力: {"name":"Hanako","age":25,"city":"Kyoto"}
```

## Deep Dive (掘り下げ)
初めてJSONが使われたのは2000年代初頭。JavaScriptのサブセットだが、多くの言語で使われている。YAMLやXMLと比べてシンプルで軽量。Rubyでは`json` ライブラリの他に高速な `oj` などの選択肢もある。内部では、文字列はUTF-8でエンコードされる。

## See Also (関連情報)
- [JSON in Ruby](https://ruby-doc.org/stdlib-2.6.3/libdoc/json/rdoc/JSON.html) - Rubyの標準ライブラリドキュメント。
- [JSON Homepage](http://json.org/) - JSON公式ホームページ, 仕様と歴史的背景。
- [Oj gem](https://github.com/ohler55/oj) - Rubyで高速なJSONパーサー。
- [Choosing the best way to serialize JSON for Ruby and Rails](https://medium.com/@adamhooper/in-search-of-the-best-json-parser-for-ruby-cdda610929be) - Ruby で JSON を扱う方法の比較。