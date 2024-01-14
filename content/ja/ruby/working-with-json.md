---
title:                "Ruby: jsonを使ったプログラミングの方法"
simple_title:         "jsonを使ったプログラミングの方法"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/working-with-json.md"
---

{{< edit_this_page >}}

# なぜ

JSONという形式でプログラミングする理由は、データを扱う上でとても便利であり、多くのウェブサイトやアプリケーションで使用されているからです。

## 方法

JSONをRubyで扱うためには、まずはRubyの標準ライブラリであるJSONをrequireする必要があります。その後、JSON.parseメソッドを使用してJSONデータをRubyオブジェクトに変換することができます。

```Ruby
require "json"

# JSONデータの例
data = '{"name": "太郎", "age": 25, "hobbies": ["料理", "旅行", "読書"]}'

# JSONをRubyオブジェクトに変換
parsed_data = JSON.parse(data)
# => {"name"=>"太郎", "age"=>25, "hobbies"=>["料理", "旅行", "読書"]}

# RubyオブジェクトをJSONに変換
json_data = parsed_data.to_json
# => "{\"name\":\"太郎\",\"age\":25,\"hobbies\":[\"料理\",\"旅行\",\"読書\"]}"
```

## 深堀り

JSONはJavaScript Object Notationの略であり、JavaScriptのオブジェクトと似たような文法を持っています。JSONはプレーンテキストであり、人間にとっても扱いやすく、またコンピューターにとっても扱いやすい形式です。また、Web APIのレスポンスとしてもよく利用されており、パースして必要なデータを抽出することができます。

## 参考リンク

- [Rubyの標準ライブラリ: JSON](https://docs.ruby-lang.org/ja/latest/library/json.html)
- [JSONチュートリアル](https://www.json.org/json-ja.html)
- [RubyでJSONを扱う方法](https://qiita.com/ryoqun/items/5ab80345d40d89e886ed)