---
title:                "yamlを使用する"
html_title:           "Kotlin: yamlを使用する"
simple_title:         "yamlを使用する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## ワット＆ホワイ
YAMLとは何かを説明するために、簡単に言えば、YAMLは人間にとって読みやすく書きやすい形式でデータを表現することができるファイル形式です。プログラマーたちは、YAMLを使用する主な理由は、データを分かりやすく整理するのに役立つからです。

## 使い方:
まず最初に、YAMLを読み込むために必要なライブラリをインポートする必要があります。次に、```Yaml```クラスを使用して、YAMLファイルを読み込み、その内容をプログラム内のオブジェクトにマッピングすることができます。具体的なコーディング例を以下に示します:

```
import org.yaml.snakeyaml.Yaml
import java.io.File

fun main() {
    val yaml = Yaml()
    val data = yaml.load(File("sample.yaml").inputStream()) //sample.yamlは読み込むYAMLファイル名に置き換えてください
    println(data["title"])
}
```

上記の例では、```sample.yaml```ファイル内の```title```キーの内容をコンソールに出力しています。コンソールには、```sample.yaml```ファイル内の```title```の値が表示されます。

## ディープダイブ:
YAMLは2001年に登場したマークアップ言語です。主な競合相手はJSONで、YAMLはより人間にとって理解しやすい書式を採用しています。YAMLは設定ファイルやデータベースのエクスポートなど、さまざまな用途で使用されています。また、JavaやPythonなどの言語でも使用することができます。

## 参照:
- [YAML公式サイト](https://yaml.org/)