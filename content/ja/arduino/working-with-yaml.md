---
title:                "YAMLを扱う"
date:                  2024-01-19
simple_title:         "YAMLを扱う"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
YAMLはデータを表すためのフォーマットです。読みやすく、人間が書きやすいため、設定やデータの保存に使われます。

## How to: (やり方)
Arduinoでは、YAMLファイルを直接扱う標準ライブラリはありませんが、独自のパーサーを実装するか、既存のライブラリを探す必要があります。以下は簡単な仮想の例です。

```Arduino
#include <YAML.h>

void setup() {
  Serial.begin(9600);
  // YAML文字列
  String yamlString = "title: 'Arduino YAML'\nversion: 1.0";

  // YAML文字列のパース
  YAML::Node config = YAML::Load(yamlString.c_str());

  // データの使用
  if (config["title"]) {
    Serial.println(config["title"].as<String>());
  }

  if (config["version"]) {
    Serial.println(config["version"].as<float>());
  }
}

void loop() {
  // ここで何かのループ処理
}
```

サンプル出力:
```
Arduino YAML
1.0
```

## Deep Dive (詳細情報)
YAMLは"YAML Ain't Markup Language"（再帰的頭字語）で、設定ファイルやデータ交換に適しています。JSONやXMLと比べて、YAMLは人間が読みやすい形式です。しかし、Arduinoにはメモリが限られているため、大きなデータ構造の扱いは難しいです。YAMLファイルの解析には、外部ライブラリが必要で、Arduinoの場合それらの中にはメモリ使用を最適化したものもあります。

## See Also (関連リンク)

- YAML公式ウェブサイト: https://yaml.org
- Arduino用のYAMLパーサーライブラリの例: https://github.com/jimmiebergmann/mini-yaml
- YAMLと他のデータフォーマットとの比較: https://en.wikipedia.org/wiki/YAML#Comparison_with_JSON
