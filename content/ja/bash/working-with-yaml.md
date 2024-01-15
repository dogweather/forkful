---
title:                "「yaml を使用する」"
html_title:           "Bash: 「yaml を使用する」"
simple_title:         "「yaml を使用する」"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## なぜ
YAMLを使ったプログラミングに取り組む理由を最大2文で説明します。

YAMLは人間にとって読みやすく、機械にとっても処理しやすいフォーマットであり、多くのアプリケーションで使用されています。そのため、開発者がデータ構造を定義し、データを格納するのに役立ちます。

## 使い方
```Bash
# YAMLファイルを作成
touch example.yaml

# vimを使用してYAMLファイルを開く
vim example.yaml

# データをYAML形式で定義
name: John Doe
age: 30
hobby:
  - hiking
  - photography

# ファイルを保存して終了
:wq

# YAMLファイルを読み込んで出力する
cat example.yaml

# 出力例
name: John Doe
age: 30
hobby:
  - hiking
  - photography
```

## より詳しく
YAMLは、キーと値を持つマッピング形式や、配列を持つシーケンス形式をサポートしています。また、インデントを使って階層構造を表すことができ、複雑なデータ構造を簡潔に表現することができます。

また、YAMLの拡張子は「.yaml」や「.yml」のように使用でき、多くのプログラミング言語でパースすることができます。

## 関連リンク
- [YAML公式サイト](https://yaml.org/)
- [YAMLチュートリアル](https://learnxinyminutes.com/docs/yaml/)
- [YAMLの名前について](https://qiita.com/tmurakami1234/items/099c5ca982d2c0dafcf3)