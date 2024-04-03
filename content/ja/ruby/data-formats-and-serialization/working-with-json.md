---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:14.599855-07:00
description: "\u65B9\u6CD5\uFF1A Ruby\u306F\u305D\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3092\u901A\u3058\u3066\u3001JSON\u306E\u89E3\u6790\u3084\u751F\u6210\
  \u3092\u30B7\u30FC\u30E0\u30EC\u30B9\u306B\u884C\u3046\u65B9\u6CD5\u3092\u63D0\u4F9B\
  \u3057\u3066\u3044\u307E\u3059\u3002\u3053\u308C\u3089\u306E\u64CD\u4F5C\u306E\u305F\
  \u3081\u306E\u4E3B\u8981\u30E2\u30B8\u30E5\u30FC\u30EB\u306F`json`\u3067\u3001\u4EFB\
  \u610F\u306ERuby\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u7C21\u5358\
  \u306B\u7D71\u5408\u3067\u304D\u307E\u3059\u3002 #."
lastmod: '2024-03-13T22:44:42.884177-06:00'
model: gpt-4-0125-preview
summary: "Ruby\u306F\u305D\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u901A\
  \u3058\u3066\u3001JSON\u306E\u89E3\u6790\u3084\u751F\u6210\u3092\u30B7\u30FC\u30E0\
  \u30EC\u30B9\u306B\u884C\u3046\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\
  \u3059\u3002\u3053\u308C\u3089\u306E\u64CD\u4F5C\u306E\u305F\u3081\u306E\u4E3B\u8981\
  \u30E2\u30B8\u30E5\u30FC\u30EB\u306F`json`\u3067\u3001\u4EFB\u610F\u306ERuby\u30A2\
  \u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u7C21\u5358\u306B\u7D71\u5408\u3067\
  \u304D\u307E\u3059."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

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
