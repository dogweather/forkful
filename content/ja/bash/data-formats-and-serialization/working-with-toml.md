---
date: 2024-01-26 04:19:03.600767-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A \u307E\u305A\u3001Bash\u3067TOML\u3092\
  \u3044\u3058\u308B\u305F\u3081\u306B`toml-cli`\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\
  \u30EB\u3057\u307E\u3059\u3002TOML\u30D5\u30A1\u30A4\u30EB\u3092\u305D\u306E\u5834\
  \u3067\u8AAD\u3093\u3060\u308A\u7DE8\u96C6\u3057\u305F\u308A\u3059\u308B\u306E\u306B\
  \u4FBF\u5229\u3067\u3059\u3002"
lastmod: '2024-04-05T21:53:43.235253-06:00'
model: gpt-4-0125-preview
summary: ''
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
weight: 39
---

## どうやって：
まず、BashでTOMLをいじるために`toml-cli`をインストールします。TOMLファイルをその場で読んだり編集したりするのに便利です。

```Bash
# TOMLタスクのための小さなヘルパー、toml-cliをインストールします
pip install toml-cli

# 'config.toml'というTOMLファイルがあるとします
echo -e 'title = "TOML Demo"\n\n[owner]\nname = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# 値を読む
toml get config.toml owner.name
# 出力：Tom

# 値を設定する
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# 豆知識：ドットや変な文字が含まれるキーには引用符を使いましょう！
```

## 深掘り
TOMLは、人間にとってのJSONの障壁に対する不満から、2013年頃に登場しました。GitHubの共同創設者であるTom Preston-Wernerは、非常に読みやすい何かを望んでいました。YAMLとINIは代替案でしたが、TOMLはその両方のベストです。

シバン！ネストされたデータや配列を持つことができ、YAMLの落とし穴やJSONの中括弧を回避します。今では、RustのCargoでの設定におけるTOMLの主流であり、それが開発界での隆盛を物語っています。しっかりと定義され、厳格に管理された仕様によって推進されており、ほぼあらゆる言語でパーサーを手に入れることができるため、幅広く採用されています。

## また見る
- 公式TOML GitHubリポジトリ：https://github.com/toml-lang/toml
- PyPI上のtoml-cli：https://pypi.org/project/toml-cli/
- データシリアライゼーション形式の比較：https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
