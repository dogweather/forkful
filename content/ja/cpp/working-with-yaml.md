---
title:                "YAML を操作する"
aliases:
- ja/cpp/working-with-yaml.md
date:                  2024-02-03T19:24:42.470224-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML を操作する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
