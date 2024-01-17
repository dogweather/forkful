---
title:                "YAMLの操作方法"
html_title:           "Fish Shell: YAMLの操作方法"
simple_title:         "YAMLの操作方法"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## 何？なんで？
YAMLを使ってプログラムを書くのは、データを簡潔に表現したり、設定ファイルを読み込んだりすることができるからです。

## 使い方：
```
Fish Shellを使って、YAMLを読み込んでデータを表示する例を見てみましょう。
```
```Fish Shell
cat file.yaml | yq r -
```
```
出力：ファイルから読み込まれたデータが表示されます。
```

## ディープダイブ：
YAMLは、JSONやXMLのようなデータフォーマットですが、さらに人間が読みやすくするために作られました。他のフォーマットと比べると、スペースや改行など、構文に厳格なルールがありません。Fish Shellでは、バッククオートを使うことでコマンドラインからYAMLをパースすることができます。また、代替として`yq`コマンドを使うこともできます。

## さらに見る：
- [Fish Shell公式ドキュメント](https://fishshell.com/docs/current/index.html)
- [Fish ShellでのYAMLの使用例](https://github.com/fish-shell/fish-shell/wiki/YAML-Usage)