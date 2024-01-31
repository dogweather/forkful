---
title:                "YAMLを扱う"
date:                  2024-01-19
simple_title:         "YAMLを扱う"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

YAMLは設定ファイルなどによく使われるデータ形式です。シンプルで人間にも読みやすく、多くの言語で簡単にパースできるため、プログラマーにとって重宝しています。

## How to: (方法)

YAMLファイルを操作する基本的なコマンドをいくつか紹介します。

```Fish Shell
# YAMLファイルをパースして変数に入れる
set config (yaml2json config.yaml | jq .)

# 特定のキーの値を出力
yaml2json config.yaml | jq '.database.host'

# 出力例: "localhost"

# YAML配列の一覧を出力
yaml2json config.yaml | jq '.users[]'

# 出力例: 
# "alice"
# "bob"
# "charlie"
```
※ `yaml2json`と`jq`コマンドをインストールして使用します。

## Deep Dive (深堀り)

YAMLは"YAML Ain't Markup Language" (元々は"Yet Another Markup Language") の略で、インデントを使ってデータの階層を表現する形式です。JSONやXMLと比べても読みやすいですが、タブ文字を使ってはならず、スペースを使います。`yaml2json`や`jq`はYAMLデータを扱う際の強力なツールで、YAMLをJSONへ変換することで、豊富なJSON操作ツールを利用できるようになります。

## See Also (関連情報)

- YAML公式サイト: [https://yaml.org](https://yaml.org)
- `jq`コマンドマニュアル: [https://stedolan.github.io/jq/manual/](https://stedolan.github.io/jq/manual/)
- Fish Shell公式ドキュメンテーション: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- `yaml2json`ソース: [https://github.com/bronze1man/yaml2json](https://github.com/bronze1man/yaml2json)
