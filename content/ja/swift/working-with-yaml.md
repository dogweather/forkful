---
title:                "yamlを使用したプログラミングの仕事"
html_title:           "Swift: yamlを使用したプログラミングの仕事"
simple_title:         "yamlを使用したプログラミングの仕事"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
YAMLとは、データの構造を定義するためのファイル形式です。プログラマーがYAMLを使用する理由は様々ですが、主な理由は可読性や柔軟性の向上です。また、様々なプログラミング言語で利用できるため、チーム内でのコラボレーションにも役立ちます。

## 方法：
```Swift 
let yamlString = """
name: John Smith
age: 35
height: 175
"""

let decoded = try YAMLEncoder().decode(YAML.self, from: yamlString)
print(decoded.name) // John Smith
print(decoded.age) // 35
print(decoded.height) // 175
```

## 深堀り：
YAMLは1990年代に開発され、YAML Ain't Markup Languageの略です。他のファイル形式と比べるとシンプルで読みやすい構文を持ち、Web開発や設定ファイルの管理など様々な用途で利用されています。代替としてはJSONやXMLがありますが、YAMLのヒューマンリーダブルな特徴は人気を集めています。また、SwiftにはYAMLの解析や生成を手軽に行うためのライブラリが多数存在します。

## 関連リンク：
- [YAML公式サイト](https://yaml.org/)
- [YAML言語仕様書](https://yaml.org/spec/1.2/spec.html)
- [Swift-YAMLライブラリ](https://github.com/behrang/YamlSwift)