---
title:                "TOMLを扱う方法"
date:                  2024-01-26T04:19:03.600767-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOMLを扱う方法"

category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/working-with-toml.md"
---

{{< edit_this_page >}}

## 何となく何故？
TOML（Tom's Obvious, Minimal Languageの略）は、データ直列化形式です。そのシンプルさと可読性からプログラマーに好まれており、設定ファイルに最適です。YAMLに似た雰囲気を持ちながらも、人間にとってJSONよりも厄介さが少ないです。

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
