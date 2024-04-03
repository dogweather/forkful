---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:42.470224-07:00
description: "\u65B9\u6CD5: C++\u3067YAML\u3092\u6271\u3046\u5834\u5408\u3001\u4E00\
  \u822C\u7684\u306A\u9078\u629E\u80A2\u306F`yaml-cpp`\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u3067\u3059\u3002\u307E\u305A\u3001C++\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306B\
  `yaml-cpp`\u304C\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3055\u308C\u3066\u304A\u308A\
  \u3001\u9069\u5207\u306B\u30EA\u30F3\u30AF\u3055\u308C\u3066\u3044\u308B\u3053\u3068\
  \u3092\u78BA\u8A8D\u3057\u3066\u304F\u3060\u3055\u3044\u3002 **YAML\u30D5\u30A1\u30A4\
  \u30EB\u3092\u8AAD\u3080:**."
lastmod: '2024-03-13T22:44:42.582240-06:00'
model: gpt-4-0125-preview
summary: "C++\u3067YAML\u3092\u6271\u3046\u5834\u5408\u3001\u4E00\u822C\u7684\u306A\
  \u9078\u629E\u80A2\u306F`yaml-cpp`\u30E9\u30A4\u30D6\u30E9\u30EA\u3067\u3059\u3002\
  \u307E\u305A\u3001C++\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306B`yaml-cpp`\u304C\u30A4\
  \u30F3\u30B9\u30C8\u30FC\u30EB\u3055\u308C\u3066\u304A\u308A\u3001\u9069\u5207\u306B\
  \u30EA\u30F3\u30AF\u3055\u308C\u3066\u3044\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\
  \u3066\u304F\u3060\u3055\u3044."
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

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
