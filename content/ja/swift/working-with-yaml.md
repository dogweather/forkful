---
title:                "YAMLを使用する"
html_title:           "Swift: YAMLを使用する"
simple_title:         "YAMLを使用する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## なぜ

あなたはもうお馴染みのJSONではなく、新しいデータフォーマットであるYAMLを使用する理由は2つあります。第一に、YAMLは人間にとっても読み書きしやすい構造を持っています。そして第二に、SwiftにはYAMLを扱うための便利なライブラリがたくさんあります。

## 使い方

まずはSwiftでYAMLをパースする方法を紹介します。必要なライブラリをインストールしたら、以下のコードを使ってYAMLファイルをパースしましょう。

```Swift
import YAML

let yamlString = """
name: John
age: 30
"""

do {
    let data = try YAMLDecoder().decode([String: String].self, from: yamlString)
    print(data)
} catch {
    print(error)
}
```
このコードを実行すると、以下のような結果が得られます。

```
["name": "John", "age": "30"]
```

次はYAMLファイルを読み込んでJSONに変換する方法です。そのためには以下のようなコードを使います。

```Swift
import YAML

let yamlUrl = URL(fileURLWithPath: "sample.yaml")

do {
    let data = try YAMLDecoder().decode([String: Any].self, from: Data(contentsOf: yamlUrl))
    let jsonData = try JSONSerialization.data(withJSONObject: data, options: .prettyPrinted)
    let jsonString = String(data: jsonData, encoding: .utf8)
    print(jsonString)
} catch {
    print(error)
}
```
実行すると、YAMLファイルの内容がJSONに変換されて出力されます。これでYAMLを扱うための基本的な知識は身についたと言えます。

## 深堀り

YAMLはJSONと同様にネストされたデータ構造を持つことができますが、シンタックスが少し異なります。また、配列やマップを明示的に書かなくてもYAMLは自動的にそれらを認識することができます。詳細な使い方やフォーマットについては公式のドキュメントを参照することをおすすめします。

## 関連記事

- [Yamsライブラリ公式サイト](https://github.com/jpsim/Yams)
- [YAMLドキュメント](https://yaml.org/)
- [YAMLとJSONの比較](https://dzone.com/articles/yaml-vs-json-1)