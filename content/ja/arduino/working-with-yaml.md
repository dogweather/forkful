---
title:                "yamlを扱う"
html_title:           "Arduino: yamlを扱う"
simple_title:         "yamlを扱う"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
プログラマーがYAMLを使用する理由は何でしょうか？YAMLとは何かについて説明します。YAMLとは、人が読み書きしやすい形式でデータを表現するための軽量な言語です。プログラマーがYAMLを使用する理由は、データを簡単に読み書きできるためです。

## 使い方：
以下は、YAMLを使用するArduinoのコーディング例とサンプル出力です。

```
Arduino library:
#include <YAML.h>
 
YAML.begin();
 
String yamlString = F("greeting: hello\nname: world");
DynamicJsonDocument data(1024);
if (deserializeYaml(data, yamlString)) {
  Serial.println(data["greeting"].as<char*>());
  Serial.println(data["name"].as<char*>());
} else {
  Serial.println("YAML parsing failed");
}

```

出力：
```
hello
world
```

## 詳しく見る：
YAMLは、XMLやJSONなどの他の形式と比較して、より人が読み書きしやすい形式を提供します。XMLは、重いタグ構造を使用してデータを表現します。JSONは、配列やオブジェクトなどの構造体を使用してデータを表現します。しかし、YAMLはシンプルなキーと値の形式を採用しています。また、インデントを使用することで、階層構造を表現することができます。

さらに、YAMLはJSONやXMLよりも柔軟性があります。コメントを挿入することができるため、人間が読みやすいドキュメントとして使用することができます。

YAMLは、さまざまなプログラミング言語でサポートされており、Arduinoでも使用することができます。しかし、代替として使用できるものもあります。JSONやXMLは、YAMLよりも広く使用されていますが、YAMLの柔軟性と人間が読みやすい形式は、特定のユースケースに向いているかもしれません。

## 他の情報を見る：
YAMLについて詳しく知りたい方は、以下のリンクを参考にしてください。

- [YAML公式サイト](http://www.yaml.org/)
- [YAMLチュートリアル](https://learnxinyminutes.com/docs/yaml/)