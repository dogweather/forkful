---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:32.770732-07:00
description: "YAML\u306F \"YAML Ain't Markup Language\"\u2026"
lastmod: '2024-03-11T00:14:16.432514-06:00'
model: gpt-4-0125-preview
summary: "YAML\u306F \"YAML Ain't Markup Language\"\u2026"
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
---

{{< edit_this_page >}}

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
