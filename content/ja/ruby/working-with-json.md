---
title:                "JSONを活用する"
date:                  2024-02-03T19:24:14.599855-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSONを活用する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

JSON（JavaScript Object Notation）は、クライアントとサーバー間のデータ交換に広く使われている軽量なデータ交換フォーマットです。RubyでのJSONの扱い方は、外部ソースから受け取ったデータの解析、またはAPIのレスポンスのためのデータ形式の作成において、その人間が読みやすい構造を活用して簡単にデータ操作や保存を行うためにプログラマーによって行われます。

## 方法：

Rubyはその標準ライブラリを通じて、JSONの解析や生成をシームレスに行う方法を提供しています。これらの操作のための主要モジュールは`json`で、任意のRubyアプリケーションに簡単に統合できます。

### JSONの解析：

JSON文字列をRubyのハッシュに変換するには、`JSON.parse`メソッドを使用します。

```ruby
require 'json'

json_string = '{"name": "John Doe", "age": 30, "city": "New York"}'
ruby_hash = JSON.parse(json_string)

puts ruby_hash
# 出力: {"name"=>"John Doe", "age"=>30, "city"=>"New York"}
```

### JSONの生成：

逆に、RubyのハッシュをJSON文字列に変換する場合は、`json`ライブラリが要求された後にRubyオブジェクトで利用可能な`JSON.generate`メソッドまたは`to_json`メソッドを使用します。

```ruby
require 'json'

ruby_hash = { name: "Jane Doe", age: 25, city: "Los Angeles" }
json_string = ruby_hash.to_json

puts json_string
# 出力: {"name":"Jane Doe","age":25,"city":"Los Angeles"}
```

### サードパーティのライブラリ：

Rubyの標準ライブラリが基本的なJSONの取り扱いをカバーしている一方で、多くのプロジェクトは機能拡張やパフォーマンス向上のためにサードパーティのライブラリに依存しています。人気の選択肢の一つが`Oj`（Optimized JSON）です。

#### Ojを用いた解析：

```ruby
require 'oj'

json_string = '{"name": "Alex", "age": 40, "city": "Chicago"}'
ruby_hash = Oj.load(json_string)

puts ruby_hash
# 出力: {"name"=>"Alex", "age"=>40, "city"=>"Chicago"}
```

#### Ojによる生成：

Ojは、RubyオブジェクトからJSONを迅速に生成する方法も提供しています：

```ruby
require 'oj'

ruby_hash = { name: "Samantha", age: 35, city: "Miami" }
json_string = Oj.dump(ruby_hash)

puts json_string
# 出力: {"name":"Samantha","age":35,"city":"Miami"}
```

これらの例は、シンプルなデータ操作から複雑なAPI通信に至るまで、RubyでのJSONの扱いが直接的な性質を持ち、アクセスしやすいことを示しています。
