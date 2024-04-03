---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:14.599855-07:00
description: "JSON\uFF08JavaScript Object\u2026"
lastmod: '2024-03-13T22:44:42.884177-06:00'
model: gpt-4-0125-preview
summary: "JSON\uFF08JavaScript Object Notation\uFF09\u306F\u3001\u30AF\u30E9\u30A4\
  \u30A2\u30F3\u30C8\u3068\u30B5\u30FC\u30D0\u30FC\u9593\u306E\u30C7\u30FC\u30BF\u4EA4\
  \u63DB\u306B\u5E83\u304F\u4F7F\u308F\u308C\u3066\u3044\u308B\u8EFD\u91CF\u306A\u30C7\
  \u30FC\u30BF\u4EA4\u63DB\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u3059\u3002Ruby\u3067\
  \u306EJSON\u306E\u6271\u3044\u65B9\u306F\u3001\u5916\u90E8\u30BD\u30FC\u30B9\u304B\
  \u3089\u53D7\u3051\u53D6\u3063\u305F\u30C7\u30FC\u30BF\u306E\u89E3\u6790\u3001\u307E\
  \u305F\u306FAPI\u306E\u30EC\u30B9\u30DD\u30F3\u30B9\u306E\u305F\u3081\u306E\u30C7\
  \u30FC\u30BF\u5F62\u5F0F\u306E\u4F5C\u6210\u306B\u304A\u3044\u3066\u3001\u305D\u306E\
  \u4EBA\u9593\u304C\u8AAD\u307F\u3084\u3059\u3044\u69CB\u9020\u3092\u6D3B\u7528\u3057\
  \u3066\u7C21\u5358\u306B\u30C7\u30FC\u30BF\u64CD\u4F5C\u3084\u4FDD\u5B58\u3092\u884C\
  \u3046\u305F\u3081\u306B\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306B\u3088\u3063\u3066\
  \u884C\u308F\u308C\u307E\u3059\u3002."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

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
