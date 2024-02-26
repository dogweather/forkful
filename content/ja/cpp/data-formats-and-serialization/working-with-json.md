---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:00.371748-07:00
description: "JSON(JavaScript Object\u2026"
lastmod: '2024-02-25T18:49:40.537952-07:00'
model: gpt-4-0125-preview
summary: "JSON(JavaScript Object\u2026"
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ?

JSON(JavaScript Object Notation)は、データを保存および転送するための軽量なフォーマットであり、サーバーとWebアプリケーション間のデータ交換のための優れた媒体です。プログラマーは、インターネットを介したデータ交換や設定の設定を必要とするアプリケーションを作業する際、人間による容易な可読性とマシンによる直接的な解析可能性のためにJSONを使用します。

## どのようにして:

C++ではJSONのネイティブサポートはありませんが、nlohmann/jsonのようなサードパーティーライブラリを使用すると簡単に行えます。基本的なタスクに使用する方法は次のとおりです:

まず、ライブラリがインストールされていることを確認してください。vcpkgやConanのようなパッケージマネージャーを使用している場合、プロジェクトに`nlohmann/json`を簡単に追加できます。

### 文字列からJSONを解析する

```cpp
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // 文字列としてのJSONデータ
    std::string jsonData = "{\"name\":\"John\", \"age\":30, \"city\":\"New York\"}";

    // JSON文字列を解析
    auto jsonObject = nlohmann::json::parse(jsonData);

    // データにアクセス
    std::cout << "Name: " << jsonObject["name"] << "\n"
              << "Age: " << jsonObject["age"] << "\n"
              << "City: " << jsonObject["city"] << std::endl;

    return 0;
}
```

**サンプル出力:**

```
Name: John
Age: 30
City: New York
```

### JSONを生成する

JSONデータの作成も同様に簡単です。単に`nlohmann::json`オブジェクトに値を割り当てます。

```cpp
#include <nlohmann/json.hpp>
#include <iostream>

int main() {
    // JSONオブジェクトを作成
    nlohmann::json jsonObject;
    jsonObject["name"] = "Jane";
    jsonObject["age"] = 25;
    jsonObject["city"] = "Los Angeles";

    // JSONオブジェクトを文字列に変換して出力
    std::string jsonString = jsonObject.dump(4); // 引数4はきれいな印刷のため
    std::cout << jsonString << std::endl;

    return 0;
}
```

**サンプル出力:**

```
{
    "name": "Jane",
    "age": 25,
    "city": "Los Angeles"
}
```

これらの例は、`nlohmann/json`ライブラリを使用してC++でJSONを扱うためのコア機能を示しています。これらの基本を使用して、設定ファイルからネットワークアプリケーションのデータ交換まで、様々なアプリケーションでJSONを解析し生成することができます。
