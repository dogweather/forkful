---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:47.984817-07:00
description: "YAML\uFF08YAML Ain't Markup\u2026"
lastmod: '2024-03-11T00:14:16.069239-06:00'
model: gpt-4-0125-preview
summary: "YAML\uFF08YAML Ain't Markup\u2026"
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
---

{{< edit_this_page >}}

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
