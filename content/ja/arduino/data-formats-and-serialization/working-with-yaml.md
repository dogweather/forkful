---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:47.984817-07:00
description: "\u65B9\u6CD5\uFF1A Arduino\u3067\u76F4\u63A5YAML\u3092\u6271\u3046\u3053\
  \u3068\u306F\u3001\u30E1\u30E2\u30EA\u5236\u7D04\u3068\u30CD\u30A4\u30C6\u30A3\u30D6\
  YAML\u51E6\u7406\u30E9\u30A4\u30D6\u30E9\u30EA\u306E\u4E0D\u5728\u306E\u305F\u3081\
  \u3001\u9AD8\u7D1A\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u74B0\u5883\u3067\u306E\
  \u64CD\u4F5C\u307B\u3069\u7C21\u5358\u3067\u306F\u3042\u308A\u307E\u305B\u3093\u3002\
  \u3057\u304B\u3057\u3001YAML\u306E\u89E3\u6790\u307E\u305F\u306F\u751F\u6210\u304C\
  \u5FC5\u8981\u306A\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306E\u5834\u5408\u3001\u4E00\
  \u822C\u7684\u306A\u30A2\u30D7\u30ED\u30FC\u30C1\u306B\u306F\u3001\u30B3\u30F3\u30D1\
  \u30CB\u30AA\u30F3\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\uFF08\u4F8B\uFF1ARaspberry\u2026"
lastmod: '2024-04-05T22:38:42.031802-06:00'
model: gpt-4-0125-preview
summary: "**\u30B9\u30C6\u30C3\u30D71:** YAML\u8A2D\u5B9A\u3092JSON\u306B\u5909\u63DB\
  \u3057\u307E\u3059\u3002\u30AA\u30F3\u30E9\u30A4\u30F3\u30C4\u30FC\u30EB\u3084\u30B3\
  \u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u30E6\u30FC\u30C6\u30A3\u30EA\u30C6\u30A3`yq`\u306A\
  \u3069\u3092\u4F7F\u7528\u3067\u304D\u307E\u3059\u3002"
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

## 方法：
Arduinoで直接YAMLを扱うことは、メモリ制約とネイティブYAML処理ライブラリの不在のため、高級プログラミング環境での操作ほど簡単ではありません。しかし、YAMLの解析または生成が必要なプロジェクトの場合、一般的なアプローチには、コンパニオンコンピュータ（例：Raspberry Pi）を使用するか、外部スクリプトを使用してYAMLファイルをよりArduinoフレンドリーな形式（例：JSON）に変換する方法が含まれます。デモンストレーション用に、後者のアプローチを人気のあるライブラリー、ArduinoJsonを使用して焦点を当てましょう。

**ステップ1:** YAML設定をJSONに変換します。オンラインツールやコマンドラインユーティリティ`yq`などを使用できます。

YAMLファイル（`config.yaml`）:
```yaml
wifi:
  ssid: "YourSSID"
  password: "YourPassword"
```

JSONに変換（`config.json`）:
```json
{
  "wifi": {
    "ssid": "YourSSID",
    "password": "YourPassword"
  }
}
```

**ステップ2:** ArduinoJsonライブラリを使用して、ArduinoスケッチでJSONファイルを解析します。まず、Arduino IDEのライブラリマネージャーを介してArduinoJsonライブラリをインストールする必要があります。

**ステップ3:** コード内でJSONを読み込み、解析します。Arduinoのストレージの制限のため、JSON文字列が変数に格納されているか、SDカードから読み出されると想像してください。

サンプルのArduinoスケッチ:
```cpp
#include <ArduinoJson.h>

const char* jsonConfig = "{\"wifi\":{\"ssid\":\"YourSSID\",\"password\":\"YourPassword\"}}";

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc;
  DeserializationError error = deserializeJson(doc, jsonConfig);

  if (error) {
    Serial.print(F("deserializeJson() failed: "));
    Serial.println(error.f_str());
    return;
  }

  const char* ssid = doc["wifi"]["ssid"]; // "YourSSID"
  const char* password = doc["wifi"]["password"]; // "YourPassword"

  Serial.print("SSID: ");
  Serial.println(ssid);
  Serial.print("Password: ");
  Serial.println(password);
}

void loop() {
  // この例ではここには何もありません
}
```

スケッチを実行すると出力：
```
SSID: YourSSID
Password: YourPassword
```

このアプローチは、JSONへの変換とArduinoJsonライブラリの利用を含み、Arduinoプロジェクト内での扱いやすいYAML設定の処理を可能にし、マイクロコントローラー上での直接的なYAML解析を回避します。
