---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:47.984817-07:00
description: "YAML\uFF08YAML Ain't Markup\u2026"
lastmod: '2024-03-13T22:44:42.521923-06:00'
model: gpt-4-0125-preview
summary: "YAML\uFF08YAML Ain't Markup Language\uFF09\u306F\u3001\u8A2D\u5B9A\u30D5\
  \u30A1\u30A4\u30EB\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u9593\u901A\u4FE1\u3001\u30C7\
  \u30FC\u30BF\u30B9\u30C8\u30EC\u30FC\u30B8\u306B\u4F7F\u7528\u3067\u304D\u308B\u3001\
  \u4EBA\u9593\u304C\u8AAD\u307F\u3084\u3059\u3044\u30C7\u30FC\u30BF\u30B7\u30EA\u30A2\
  \u30E9\u30A4\u30BA\u6A19\u6E96\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306E\u8A2D\u5B9A\u30D7\
  \u30ED\u30BB\u30B9\u3092\u5408\u7406\u5316\u3057\u3001\u30B3\u30FC\u30C9\u3092\u6DF1\
  \u304F\u6398\u308A\u4E0B\u3052\u308B\u3053\u3068\u306A\u304F\u30D1\u30E9\u30E1\u30FC\
  \u30BF\u3092\u5909\u66F4\u3057\u3084\u3059\u304F\u3059\u308B\u305F\u3081\u3001\u8AAD\
  \u307F\u3084\u3059\u3055\u3092\u5411\u4E0A\u3055\u305B\u3001\u8A2D\u5B9A\u306E\u5171\
  \u6709\u3092\u7C21\u5358\u306B\u3059\u308B\u305F\u3081\u306BArduino\u30D7\u30ED\u30B8\
  \u30A7\u30AF\u30C8\u3067YAML\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002."
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

## 何となく？

YAML（YAML Ain't Markup Language）は、設定ファイル、プログラム間通信、データストレージに使用できる、人間が読みやすいデータシリアライズ標準です。プログラマーは、アプリケーションの設定プロセスを合理化し、コードを深く掘り下げることなくパラメータを変更しやすくするため、読みやすさを向上させ、設定の共有を簡単にするためにArduinoプロジェクトでYAMLを使用します。

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
