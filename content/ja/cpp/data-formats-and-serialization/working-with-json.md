---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:00.371748-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066: C++\u3067\u306FJSON\u306E\
  \u30CD\u30A4\u30C6\u30A3\u30D6\u30B5\u30DD\u30FC\u30C8\u306F\u3042\u308A\u307E\u305B\
  \u3093\u304C\u3001nlohmann/json\u306E\u3088\u3046\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\
  \u30C6\u30A3\u30FC\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3059\u308B\u3068\
  \u7C21\u5358\u306B\u884C\u3048\u307E\u3059\u3002\u57FA\u672C\u7684\u306A\u30BF\u30B9\
  \u30AF\u306B\u4F7F\u7528\u3059\u308B\u65B9\u6CD5\u306F\u6B21\u306E\u3068\u304A\u308A\
  \u3067\u3059:\u2026"
lastmod: '2024-03-13T22:44:42.583192-06:00'
model: gpt-4-0125-preview
summary: "C++\u3067\u306FJSON\u306E\u30CD\u30A4\u30C6\u30A3\u30D6\u30B5\u30DD\u30FC\
  \u30C8\u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001nlohmann/json\u306E\u3088\u3046\
  \u306A\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30FC\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u3092\u4F7F\u7528\u3059\u308B\u3068\u7C21\u5358\u306B\u884C\u3048\u307E\u3059\u3002\
  \u57FA\u672C\u7684\u306A\u30BF\u30B9\u30AF\u306B\u4F7F\u7528\u3059\u308B\u65B9\u6CD5\
  \u306F\u6B21\u306E\u3068\u304A\u308A\u3067\u3059."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

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
