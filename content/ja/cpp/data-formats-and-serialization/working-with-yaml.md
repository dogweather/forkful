---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:42.470224-07:00
description: "YAML\u306F\u3001\u300CYAML Ain't Markup Language\u300D\u3092\u610F\u5473\
  \u3057\u3001\u4EBA\u9593\u304C\u8AAD\u307F\u3084\u3059\u3044\u30C7\u30FC\u30BF\u76F4\
  \u5217\u5316\u5F62\u5F0F\u3067\u3059\u3002\u305D\u306E\u8AAD\u307F\u3084\u3059\u3055\
  \u3068\u7406\u89E3\u3057\u3084\u3059\u3044\u69CB\u6587\u306E\u304A\u304B\u3052\u3067\
  \u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\
  \u3001\u30C7\u30FC\u30BF\u30C0\u30F3\u30D7\u3001\u968E\u5C64\u30C7\u30FC\u30BF\u306E\
  \u4FDD\u5B58\u306BXML\u3084JSON\u3068\u6BD4\u3079\u3066YAML\u3092\u4F7F\u7528\u3057\
  \u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.582240-06:00'
model: gpt-4-0125-preview
summary: "YAML\u306F\u3001\u300CYAML Ain't Markup Language\u300D\u3092\u610F\u5473\
  \u3057\u3001\u4EBA\u9593\u304C\u8AAD\u307F\u3084\u3059\u3044\u30C7\u30FC\u30BF\u76F4\
  \u5217\u5316\u5F62\u5F0F\u3067\u3059\u3002\u305D\u306E\u8AAD\u307F\u3084\u3059\u3055\
  \u3068\u7406\u89E3\u3057\u3084\u3059\u3044\u69CB\u6587\u306E\u304A\u304B\u3052\u3067\
  \u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\
  \u3001\u30C7\u30FC\u30BF\u30C0\u30F3\u30D7\u3001\u968E\u5C64\u30C7\u30FC\u30BF\u306E\
  \u4FDD\u5B58\u306BXML\u3084JSON\u3068\u6BD4\u3079\u3066YAML\u3092\u4F7F\u7528\u3057\
  \u307E\u3059\u3002"
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

## 何となぜ？

YAMLは、「YAML Ain't Markup Language」を意味し、人間が読みやすいデータ直列化形式です。その読みやすさと理解しやすい構文のおかげで、プログラマーは設定ファイル、データダンプ、階層データの保存にXMLやJSONと比べてYAMLを使用します。

## 方法:

C++でYAMLを扱う場合、一般的な選択肢は`yaml-cpp`ライブラリです。まず、C++プロジェクトに`yaml-cpp`がインストールされており、適切にリンクされていることを確認してください。

**YAMLファイルを読む:**

```cpp
#include <iostream>
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Node config = YAML::LoadFile("config.yaml");
    
    if(config["title"]) {
        std::cout << "タイトル: " << config["title"].as<std::string>() << std::endl;
    }
    
    return 0;
}
```

このような`config.yaml`があった場合：

```yaml
title: "Example YAML"
```

上記のC++コードを実行すると、次のようになります：

```
タイトル: Example YAML
```

**YAMLファイルへ書き込む:**

```cpp
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "title" << YAML::Value << "Example YAML";
    out << YAML::EndMap;
    
    std::ofstream fout("output.yaml");
    fout << out.c_str();
    
    return 0;
}
```

このコードは、以下の内容の`output.yaml`を作成します：

```yaml
title: Example YAML
```

これらの例は、`yaml-cpp`ライブラリを使ったC++でのYAMLファイルの読み書きへの基本的な導入です。より複雑な構造や使用例については、シーケンス、タグ、さらに進んだシリアライゼーションとデシリアライゼーションの技術などの機能について、`yaml-cpp`のドキュメントを探求してください。
