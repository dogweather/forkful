---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:32.770732-07:00
description: "YAML\u306F \"YAML Ain't Markup Language\"\u2026"
lastmod: '2024-03-13T22:44:42.883370-06:00'
model: gpt-4-0125-preview
summary: "YAML\u306F \"YAML Ain't Markup Language\" \u306E\u7565\u3067\u3059\u3002\
  \u3053\u308C\u306F\u3001\u305D\u306E\u4EBA\u9593\u304C\u8AAD\u307F\u3084\u3059\u3044\
  \u5F62\u5F0F\u306E\u305F\u3081\u3001Ruby\u306B\u304A\u3051\u308B\u8A2D\u5B9A\u30D5\
  \u30A1\u30A4\u30EB\u3084\u30C7\u30FC\u30BF\u306E\u76F4\u5217\u5316\u306B\u5E83\u304F\
  \u4F7F\u7528\u3055\u308C\u3066\u3044\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u30C7\u30FC\u30BF\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\u8AAD\
  \u307F\u3084\u3059\u304F\u3001\u304B\u3064\u69CB\u9020\u7684\u306B\u683C\u7D0D\u307E\
  \u305F\u306F\u8EE2\u9001\u3059\u308B\u5FC5\u8981\u304C\u3042\u308B\u5834\u5408\u306B\
  \u3001YAML\u3092\u597D\u3093\u3067\u4F7F\u7528\u3057\u307E\u3059\u3002\u3053\u308C\
  \u306F\u3001\u8A2D\u5B9A\u7BA1\u7406\u3001\u30C7\u30FC\u30BF\u30B9\u30C8\u30EC\u30FC\
  \u30B8\u3001\u7570\u8A00\u8A9E\u9593\u30C7\u30FC\u30BF\u5171\u6709\u3068\u3044\u3063\
  \u305F\u30BF\u30B9\u30AF\u3092\u7C21\u7D20\u5316\u3057\u307E\u3059\u3002."
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

## 何となぜ？
YAMLは "YAML Ain't Markup Language" の略です。これは、その人間が読みやすい形式のため、Rubyにおける設定ファイルやデータの直列化に広く使用されています。プログラマーは、データオブジェクトを読みやすく、かつ構造的に格納または転送する必要がある場合に、YAMLを好んで使用します。これは、設定管理、データストレージ、異言語間データ共有といったタスクを簡素化します。

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
