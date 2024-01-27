---
title:                "YAMLを扱う"
date:                  2024-01-19
html_title:           "Bash: YAMLを扱う"
simple_title:         "YAMLを扱う"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (何とは何ですか？そして、なぜですか？)
YAMLは設定ファイルやデータ交換のためのデータ形式です。プログラマーは読みやすさと使いやすさのためにYAMLを扱います。

## How to: (やり方)
YAMLファイルの読み書きや解析にはコマンドラインツールやBashスクリプトを使います。以下に簡単な例を示します。

```Bash
# YAMLファイルのパースには'yq'コマンドを使用することが多い
echo "name: Taro
age: 20
occupation: Engineer" > example.yaml

# 'yq'で要素を取得
yq e '.name' example.yaml
```
出力:
```
Taro
```

## Deep Dive (深掘り)
YAML (YAML Ain't Markup Language) は2001年に登場しました。JSONやXMLと同様にデータを表すのに使われていますが、その可読性の高さから設定ファイルなどで広く利用されています。YAMLを扱うツールには'yq'の他にもPythonのPyYAMLやRubyのPsychなどがあります。

## See Also (関連情報)
- [YAML公式ウェブサイト](https://yaml.org)
- [yq GitHubリポジトリ](https://github.com/mikefarah/yq)
- [PyYAMLドキュメント](https://pyyaml.org/wiki/PyYAMLDocumentation)
