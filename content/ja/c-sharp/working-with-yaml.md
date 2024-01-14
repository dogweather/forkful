---
title:                "C#: 「YAMLとの作業」"
simple_title:         "「YAMLとの作業」"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## なぜYAMLを使うのか

YAMLは、コードをシンプルで可読性の高い形式で記述することができるため、多くの開発者にとって魅力的な選択肢です。また、YAMLは複数のプログラミング言語で利用可能なため、プロジェクトの移植性にも優れています。

## 使い方

以下のC#コードを使用して、YAMLファイルの読み込みと書き込みがどのように行われるかを見てみましょう。

```C#
// YAMLファイルの読み込み
var yaml = new YamlStream();
using (var reader = new StreamReader("config.yaml"))
{
    yaml.Load(reader);
}
// 読み込んだデータをオブジェクトとしてアクセス
var root = (YamlMappingNode)yaml.Documents[0].RootNode;
string confValue = root["key1"].ToString();

// YAMLファイルの書き込み
var newYaml = new YamlStream();
using (var writer = new StreamWriter("newconfig.yaml"))
{
    newYaml.Save(writer);
}
```

```
// config.yamlファイルの内容
key1: value1
key2: value2
```

```
// newconfig.yamlファイルの内容
key1: value1
key2: value2
```

## ディープダイブ

YAMLは、さまざまなデータ構造を表現することができ、JSONよりも柔軟性があります。また、YAMLはコメントやインクルードなど、さまざまな便利な機能を提供しています。さらに、YAMLはタグを使用してデータ型を定義することもでき、データの整合性を保つことができます。

## 参考リンク

- [YAML公式サイト](https://yaml.org/)
- [YamlDotNetドキュメンテーション](https://www.yamldotnet.com/documentation/)
- [C#でYAMLを使用する方法](https://www.codeproject.com/Articles/1161061/How-to-use-YAML-in-your-Csharp-applications)