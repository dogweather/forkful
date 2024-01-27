---
title:                "YAMLを扱う"
date:                  2024-01-19
html_title:           "Bash: YAMLを扱う"
simple_title:         "YAMLを扱う"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?（何とその理由？）
C++でYAML（YAML Ain't Markup Language）は、設定ファイルやデータ交換のために使われる。読みやすく、人間にもコンピュータにもフレンドリーだからだ。

## How to:（方法）
まず、YAML-CPPライブラリを使ってみよう。これはC++のためのYAMLパーサーやエミッタだ。

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>
#include <fstream>
#include <string>

// ΥAMLファイルを読み込む
void loadYAML(const std::string &filename) {
    YAML::Node config = YAML::LoadFile(filename);

    if (config["title"]) {
        std::cout << "Title: " << config["title"].as<std::string>() << std::endl;
    }

    if (config["owner"]["name"]) {
        std::cout << "Owner Name: " << config["owner"]["name"].as<std::string>() << std::endl;
    }
}

// メイン関数
int main() {
    // 例としてsample.yamlファイルを読む。
    loadYAML("sample.yaml");

    return 0;
}
```

上記のコードは、`sample.yaml`ファイルをロードし、`title`と`owner.name`を出力する。

## Deep Dive（深掘り）
YAMLは2001年に登場。JSONやXMLより読みやすく、シンプルだ。C++でYAMLを扱うときは、YAML-CPPやBoost.YAMLがあるが、YAML-CPPのほうが人気。C++でYAMLを扱う場合は、パフォーマンスとライブラリの依存性に気をつけよう。

## See Also（関連情報）
- YAML-CPP GitHubページ: https://github.com/jbeder/yaml-cpp
- YAML公式サイト: https://yaml.org
- Boost.YAML GitHubページ: https://github.com/boostorg/yaml
- YAML 1.2 Spec: https://yaml.org/spec/1.2/spec.html

YAMLを学び、使いこなすには上のリンクが役立つだろう。
