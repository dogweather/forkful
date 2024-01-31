---
title:                "YAMLを扱う"
date:                  2024-01-19
simple_title:         "YAMLを扱う"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
(なにとなぜ？)
YAMLはデータのシリアライズ用フォーマット。設定ファイルやデータ転送に使われる。読みやすく、人間もコンピュータも扱いやすいため人気がある。

## How to:
(やり方)
RubyでYAMLを使うには、まず`yaml`ライブラリを読み込む。

```Ruby
require 'yaml'

# YAML形式の文字列
yaml_string = <<-YAML
name: Tanaka
age: 30
job: Developer
YAML

# 文字列をYAMLとしてロードし、Rubyオブジェクトに変換
person_data = YAML.load(yaml_string)

# Yamlファイル読み込み
person_data = YAML.load_file('path_to_file.yml')

# RubyオブジェクトをYAMLにダンプ
puts person_data.to_yaml
```

Sample Output:

```yaml
---
name: Tanaka
age: 30
job: Developer
```

## Deep Dive:
(深掘り)
YAMLは"YAML Ain't Markup Language"の略で、XMLやJSONと同じデータ記述言語。設定ファイルに多用される。Rubyでは、`Psych`ライブラリがYAMLパーサーとして標準装備されている。代わりにJSONやXMLを使うこともできるが、YAMLは形式が直感的で扱いやすい。

## See Also:
(関連情報)
- YAML公式サイト: [https://yaml.org](https://yaml.org)
- YAML入門ガイド: [https://docs.ansible.com/ansible/latest/reference_appendices/YAMLSyntax.html](https://docs.ansible.com/ansible/latest/reference_appendices/YAMLSyntax.html)
