---
aliases:
- /ja/arduino/working-with-json/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:51.501739-07:00
description: "JSON\u3001\u307E\u305F\u306FJavaScript\u30AA\u30D6\u30B8\u30A7\u30AF\
  \u30C8\u8868\u8A18\u306F\u3001\u8EFD\u91CF\u306A\u30C7\u30FC\u30BF\u4EA4\u63DB\u30D5\
  \u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u3042\u308A\u3001Arduino\u30D7\u30ED\u30B8\u30A7\
  \u30AF\u30C8\u306E\u30C7\u30FC\u30BF\u4FDD\u5B58\u3084\u8A2D\u5B9A\u30D5\u30A1\u30A4\
  \u30EB\u306B\u6700\u9069\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001\u305D\u306E\u5358\u7D14\u3055\u3068\u53EF\u8AAD\u6027\u304C\u3055\u307E\u3056\
  \u307E\u306A\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u74B0\u5883\u3001Arduino\u3092\
  \u542B\u3080\u3001\u3067\u306E\u4F7F\u7528\u3092\u53EF\u80FD\u306B\u3057\u3001Web\u2026"
lastmod: 2024-02-18 23:08:55.171810
model: gpt-4-0125-preview
summary: "JSON\u3001\u307E\u305F\u306FJavaScript\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\
  \u8868\u8A18\u306F\u3001\u8EFD\u91CF\u306A\u30C7\u30FC\u30BF\u4EA4\u63DB\u30D5\u30A9\
  \u30FC\u30DE\u30C3\u30C8\u3067\u3042\u308A\u3001Arduino\u30D7\u30ED\u30B8\u30A7\u30AF\
  \u30C8\u306E\u30C7\u30FC\u30BF\u4FDD\u5B58\u3084\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\
  \u306B\u6700\u9069\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u305D\u306E\u5358\u7D14\u3055\u3068\u53EF\u8AAD\u6027\u304C\u3055\u307E\u3056\u307E\
  \u306A\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u74B0\u5883\u3001Arduino\u3092\u542B\
  \u3080\u3001\u3067\u306E\u4F7F\u7528\u3092\u53EF\u80FD\u306B\u3057\u3001Web\u2026"
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？

JSON、またはJavaScriptオブジェクト表記は、軽量なデータ交換フォーマットであり、Arduinoプロジェクトのデータ保存や設定ファイルに最適です。プログラマーは、その単純さと可読性がさまざまなプログラミング環境、Arduinoを含む、での使用を可能にし、Web APIや他のシステムとのデータ交換をスムーズに行えるためにこれを使用しています。

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
