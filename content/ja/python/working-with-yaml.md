---
title:                "YAMLを扱う"
date:                  2024-01-19
html_title:           "Bash: YAMLを扱う"
simple_title:         "YAMLを扱う"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (それは何？なぜ使うの？)
YAMLは設定ファイルやデータの保存に使われるシンプルなフォーマット。読みやすく、人間が書きやすいため、多くのプログラマーに選ばれています。

## How to: (やり方)
PythonでYAMLを取り扱うには`PyYAML`ライブラリが必要です。インストール後、YAMLを読み書きできます。

```Python
# PyYAMLのインストール
# pip install PyYAML

import yaml

# YAML文字列をパースする
yaml_data = """
fruits:
  - Apple
  - Orange
  - Banana
colors:
  - Red
  - Orange
  - Yellow
"""
data = yaml.safe_load(yaml_data)
print(data['fruits'])  # 結果: ['Apple', 'Orange', 'Banana']

# PythonのディクショナリをYAML文字列に変換する
dict_data = {'pets': ['Dog', 'Cat'], 'numbers': [1, 2, 3]}
yaml_str = yaml.dump(dict_data)
print(yaml_str)
# 結果:
# numbers:
# - 1
# - 2
# - 3
# pets:
# - Dog
# - Cat
```

## Deep Dive (詳細な情報)
YAMLは"YAML Ain't Markup Language"の略で、2001年にリリースされました。JSONやXMLと比較して、人が読み書きしやすいのが特徴ですが、パーサーが複雑になる可能性もあります。`PyYAML`はPythonでYAMLを扱う一番ポピュラーなライブラリで、セキュリティやパフォーマンスに配慮しながらYAML操作を行います。

## See Also (関連リンク)
- [YAML公式サイト](https://yaml.org/)
- [PyYAMLドキュメント](https://pyyaml.org/wiki/PyYAMLDocumentation)
- [YAMLとJSONの比較](https://en.wikipedia.org/wiki/YAML#Comparison_with_JSON)
