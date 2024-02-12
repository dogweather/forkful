---
title:                "JSONを活用する"
aliases:
- /ja/arduino/working-with-json.md
date:                  2024-02-03T19:21:51.501739-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSONを活用する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
