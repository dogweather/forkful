---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:32.770732-07:00
description: "\u4F7F\u3044\u65B9: Ruby\u306FPsych\u3068\u547C\u3070\u308C\u308B\u7D44\
  \u307F\u8FBC\u307F\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u542B\u3093\u3067\u304A\u308A\
  \u3001YAML\u306E\u89E3\u6790\u3068\u51FA\u529B\u304C\u53EF\u80FD\u3067\u3059\u3002\
  \u3053\u308C\u3092\u4F7F\u7528\u3059\u308B\u306B\u306F\u3001\u307E\u305AYAML\u6A19\
  \u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u8981\u6C42\u3059\u308B\u5FC5\u8981\u304C\
  \u3042\u308A\u307E\u3059\u3002\u3053\u3053\u306B\u57FA\u672C\u7684\u306A\u4F8B\u3092\
  \u793A\u3057\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.883370-06:00'
model: gpt-4-0125-preview
summary: "Ruby\u306FPsych\u3068\u547C\u3070\u308C\u308B\u7D44\u307F\u8FBC\u307F\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u3092\u542B\u3093\u3067\u304A\u308A\u3001YAML\u306E\u89E3\
  \u6790\u3068\u51FA\u529B\u304C\u53EF\u80FD\u3067\u3059\u3002\u3053\u308C\u3092\u4F7F\
  \u7528\u3059\u308B\u306B\u306F\u3001\u307E\u305AYAML\u6A19\u6E96\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3092\u8981\u6C42\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\
  \u3002\u3053\u3053\u306B\u57FA\u672C\u7684\u306A\u4F8B\u3092\u793A\u3057\u307E\u3059\
  \uFF1A."
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

## 使い方:
RubyはPsychと呼ばれる組み込みライブラリを含んでおり、YAMLの解析と出力が可能です。これを使用するには、まずYAML標準ライブラリを要求する必要があります。ここに基本的な例を示します：

```ruby
require 'yaml'

# シリアライズされたハッシュ
person = { name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"] }

# ハッシュをYAMLに変換
yaml_data = person.to_yaml

puts yaml_data
```

**サンプル出力:**

```yaml
---
:name: John Doe
:age: 30
:skills:
- Ruby
- JavaScript
```

YAMLデータをRubyオブジェクトに戻すには：

```ruby
loaded_person = YAML.load(yaml_data)

puts loaded_person
```

**サンプル出力:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

### サードパーティ製ライブラリの使用:
標準ライブラリは基本的なタスクには十分ですが、より複雑なニーズに対しては、'safe_yaml'のようなサードパーティ製のgemを検討するかもしれません。このようなライブラリを使用するには、まずgemをインストールする必要があります：

```bash
gem install safe_yaml
```

その後、ユーザー制御のソースからのオブジェクトインスタンス化のようなリスクを緩和しながら、安全にYAMLデータをロードするためにそれを使用できます：

```ruby
require 'safe_yaml'

safe_loaded_person = SafeYAML.load(yaml_data)

puts safe_loaded_person
```

**サンプル出力:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

このアプローチは、信頼できないソースからYAMLをロードするアプリケーションにとって良い選択肢となり、YAMLの取り扱いのセキュリティを強化します。
