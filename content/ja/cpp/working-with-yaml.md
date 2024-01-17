---
title:                "「Yaml についての作業」"
html_title:           "C++: 「Yaml についての作業」"
simple_title:         "「Yaml についての作業」"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
YAMLとは何か、プログラマーがそれをする理由について説明します。YAMLはHuman-readableなデータシリアライゼーションフォーマットです。プログラマーがデータを読みやすく、扱いやすくするために使われます。

## 方法：
下記の例はC++でYAMLを使う方法を示しています。`` `C++ ... ` ``コードブロック内にコーディング例と出力があります。

`` `C++
#include <yaml-cpp/yaml.h>
#include <iostream>

int main() {
  // Creating YAML node for an array
  YAML::Node node = YAML::Load("[1, 2, 3]");

  // Accessing elements in the array
  std::cout << "First element: " << node[0].as<int>() << std::endl;
  std::cout << "Second element: " << node[1].as<int>() << std::endl;
  std::cout << "Third element: " << node[2].as<int>() << std::endl;
  return 0;
}
`` ` ``

出力：

`` ``
First element: 1
Second element: 2
Third element: 3
`` ``

## 深く掘り下げる：
YAMLは2001年に開発されたプログラミング言語であるPerlに由来しています。XMLやJSONと比べると、シンタックスがシンプルでかつ複雑な階層構造をサポートできる点が特徴です。代替としては、JSONやXMLが挙げられます。YAMLはコード内で使用することで、設定ファイルやデータベースのようなデータを簡単に読み書きすることができます。

## 関連リンク：
- YAMLオフィシャルサイト： https://yaml.org/
- YAMLとは？： https://ja.wikipedia.org/wiki/YAML
- YAML-CPPライブラリ： https://github.com/jbeder/yaml-cpp