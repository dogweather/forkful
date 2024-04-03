---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:00.371748-07:00
description: "JSON(JavaScript Object\u2026"
lastmod: '2024-03-13T22:44:42.583192-06:00'
model: gpt-4-0125-preview
summary: "JSON(JavaScript Object Notation)\u306F\u3001\u30C7\u30FC\u30BF\u3092\u4FDD\
  \u5B58\u304A\u3088\u3073\u8EE2\u9001\u3059\u308B\u305F\u3081\u306E\u8EFD\u91CF\u306A\
  \u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u3042\u308A\u3001\u30B5\u30FC\u30D0\u30FC\
  \u3068Web\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u9593\u306E\u30C7\u30FC\
  \u30BF\u4EA4\u63DB\u306E\u305F\u3081\u306E\u512A\u308C\u305F\u5A92\u4F53\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A4\u30F3\u30BF\u30FC\u30CD\
  \u30C3\u30C8\u3092\u4ECB\u3057\u305F\u30C7\u30FC\u30BF\u4EA4\u63DB\u3084\u8A2D\u5B9A\
  \u306E\u8A2D\u5B9A\u3092\u5FC5\u8981\u3068\u3059\u308B\u30A2\u30D7\u30EA\u30B1\u30FC\
  \u30B7\u30E7\u30F3\u3092\u4F5C\u696D\u3059\u308B\u969B\u3001\u4EBA\u9593\u306B\u3088\
  \u308B\u5BB9\u6613\u306A\u53EF\u8AAD\u6027\u3068\u30DE\u30B7\u30F3\u306B\u3088\u308B\
  \u76F4\u63A5\u7684\u306A\u89E3\u6790\u53EF\u80FD\u6027\u306E\u305F\u3081\u306BJSON\u3092\
  \u4F7F\u7528\u3057\u307E\u3059\u3002."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

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
