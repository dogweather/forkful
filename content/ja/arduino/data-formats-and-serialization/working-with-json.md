---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:51.501739-07:00
description: "\u65B9\u6CD5\uFF1A Arduino\u3067JSON\u3092\u6271\u3046\u306B\u306F\u3001\
  `ArduinoJson`\u30E9\u30A4\u30D6\u30E9\u30EA\u304C\u4F7F\u3044\u3084\u3059\u3055\u3068\
  \u52B9\u7387\u306E\u305F\u3081\u306B\u4EBA\u6C17\u306E\u9078\u629E\u3067\u3059\u3002\
  \u3053\u308C\u306B\u3088\u308A\u3001JSON\u6587\u5B57\u5217\u306E\u89E3\u6790\u3001\
  \u305D\u308C\u3089\u306E\u5909\u66F4\u3001\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\
  JSON\u6587\u5B57\u5217\u306B\u30B7\u30EA\u30A2\u30E9\u30A4\u30BA\u3059\u308B\u3053\
  \u3068\u304C\u53EF\u80FD\u3067\u3059\u3002\u4F7F\u3044\u65B9\u306F\u4EE5\u4E0B\u306E\
  \u901A\u308A\u3067\u3059\uFF1A 1.\u2026"
lastmod: '2024-04-05T21:53:43.339146-06:00'
model: gpt-4-0125-preview
summary: "**ArduinoJson\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u30A4\u30F3\u30B9\u30C8\
  \u30FC\u30EB**\uFF1AArduino IDE\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u30DE\u30CD\u30FC\
  \u30B8\u30E3\u30FC\u3092\u4F7F\u7528\u3057\u3001\u300CArduinoJson\u300D\u3092\u30A4\
  \u30F3\u30B9\u30C8\u30FC\u30EB\u3057\u307E\u3059."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

## 方法：
ArduinoでJSONを扱うには、`ArduinoJson`ライブラリが使いやすさと効率のために人気の選択です。これにより、JSON文字列の解析、それらの変更、オブジェクトをJSON文字列にシリアライズすることが可能です。使い方は以下の通りです：

1. **ArduinoJsonライブラリをインストール**：Arduino IDEのライブラリマネージャーを使用し、「ArduinoJson」をインストールします。

2. **JSON文字列のデシリアライズ**：以下は、JSON文字列を解析し、値を抽出する方法です。

```cpp
#include <ArduinoJson.h>

const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

void setup() {
  Serial.begin(9600);
  StaticJsonDocument<200> doc; // JSONドキュメントに応じてサイズを調整
  DeserializationError error = deserializeJson(doc, json);

  if (error) {
    Serial.print(F("deserializeJson() failed: "));
    Serial.println(error.f_str());
    return;
  }

  const char* sensor = doc["sensor"]; // "gps"
  long time = doc["time"]; // 1351824120
  float latitude = doc["data"][0]; // 48.756080
  float longitude = doc["data"][1]; // 2.302038
  
  Serial.println(sensor);
  Serial.println(time);
  Serial.println(latitude, 6);
  Serial.println(longitude, 6);
}

void loop() {
  // 空ループ
}
```

サンプル出力：

```
gps
1351824120
48.756080
2.302038
```

3. **JSON文字列へのシリアライズ**：以下は、データからJSON文字列を作成する方法です。

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc; // データに応じてサイズを調整
  doc["sensor"] = "gps";
  doc["time"] = 1351824120;
  JsonArray data = doc.createNestedArray("data");
  data.add(48.756080);
  data.add(2.302038);

  serializeJson(doc, Serial);
}

void loop() {
  // 空ループ
}
```

サンプル出力（読みやすくするためにフォーマット）：

```
{"sensor":"gps","time":1351824120,"data":[48.756080,2.302038]}
```

`ArduinoJson` ライブラリの効果的な使用により、Arduinoプロジェクトは人間が読めるフォーマットで複雑なデータ構造を通信でき、Webサービスとの開発及び統合が容易になります。
