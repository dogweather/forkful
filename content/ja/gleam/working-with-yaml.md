---
title:                "Gleam: 「YAML を使う」"
simple_title:         "「YAML を使う」"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

# なぜYAMLを使うのか

YAMLは人間にとって読みやすく、コンピューターにとっても処理しやすいデータの形式です。Gleamでは、YAMLを使用して構成ファイルやデータ構造を作成することができます。これにより、アプリケーションの設定やデータの管理がより簡単になります。

## 方法

まずは、GleamでYAMLを使用する方法をご紹介します。まず、YAMLライブラリをインポートしましょう。

```Gleam
import yaml
```

次に、YAML形式のテキストをパースするための関数を使用します。

```Gleam
let data = yaml.parse("name: John\nage: 30\nhobbies:\n - coding\n - hiking\n - cooking")
```

このコードでは、YAML形式のテキストを解析して、データを扱いやすい形式に変換します。実行すると、次のようなデータが得られます。

```Gleam
{
  name: "John",
  age: 30,
  hobbies: ["coding","hiking","cooking"]
}
```

このように、YAMLを使用することで、データをより構造化し、扱いやすくすることができます。

## 深堀り

YAMLは非常に柔軟なデータ形式であり、さまざまな機能を備えています。例えば、キーと値のペアを含むマップだけでなく、リストや文字列といったさまざまなデータ型を表現することができます。

また、インデントを使用してデータの階層構造を表現することもできます。このように、様々な方法でデータを表現することができるため、GleamではYAMLがよく使用されます。

## 併せて読みたい

- [公式YAML仕様書](https://yaml.org/spec/)
- [GleamのYAMLドキュメンテーション](https://gleam.run/packages/yaml/)
- [YAMLを使用したGleamアプリケーションの例](https://github.com/gleam-lang/gleam/blob/master/examples/yaml_example.yaml)

ありがとうございました。YAMLを使用して、より簡単にデータを管理し、アプリケーションを構築しましょう！