---
title:                "Fish Shell: YAMLを扱う"
simple_title:         "YAMLを扱う"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## なぜYAMLを使用するのか？

YAMLは簡易で人間に読める形式でデータを管理することができるため、プログラミングでよく使用されます。また、データの変更や追加が容易で、複数のプログラムや言語間でも互換性があります。

## ヤムルをフィッシュシェルで使用する方法

```Fish Shell
# YAMLファイルの読み込み
set yaml_data (yq read config.yml)

# 変数から値を取得
set key value
set value $yaml_data[key]

# テキストとしてYAMLを出力
yq read config.yml
```

## ヤムルのディープダイブ

YAMLはインデントによってデータの階層構造を表現するため、より複雑なデータを管理することができます。また、データの型も自由に設定することができるため、柔軟性があります。

## 参考リンク

- YAML公式サイト：https://yaml.org/
- Fish Shell公式サイト：https://fishshell.com/
- yqドキュメンテーション：https://github.com/kislyuk/yq#examples