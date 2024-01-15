---
title:                "「Jsonとの作業」"
html_title:           "Arduino: 「Jsonとの作業」"
simple_title:         "「Jsonとの作業」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## なぜJSONを使用するのか
JSONは、複数のセンサーやデバイスからのデータを処理する際に非常に便利です。Arduinoでは、JSONを使用してデバイスからのデータを取得して処理することができます。

## 使い方
```Arduino
// JSONライブラリのインポート
#include <ArduinoJson.h>

// 受信したJSONデータを保存する変数
StaticJsonDocument<200> json_data;

// 受信したデータを変数に保存する関数
void saveData(String data){
  // 受信したデータをJSONオブジェクトに変換
  DeserializationError error = deserializeJson(json_data, data);
  if (error) {
    Serial.print("Error: ");
    Serial.println(error.c_str());
    return;
  }
  // 変数にデータを保存
  int value = json_data["value"];
}

void setup(){
  // シリアル通信の設定
  Serial.begin(9600);
}

void loop(){
  // シリアル通信から受信したデータを保存
  String data = Serial.readStringUntil('\n');
  // データを処理する関数を呼び出し
  saveData(data);
}
```

このコードでは、Serialから受信したJSONデータを変数に保存しています。JSONオブジェクトにデータを変換するために、ArduinoJsonライブラリを使用しています。データを保存した後は、変数を使用してデータを処理することができます。

## 詳細について
JSONは、複数のデータ形式をサポートし、インデックスを使用してデータを管理することができます。そのため、複雑なデータの処理や解析を行う際に非常に便利です。また、ArduinoJsonライブラリは、JSONデータをパースするための高速で効率的な方法を提供しています。

## 参考リンク
- [ArduinoJsonライブラリ](https://arduinojson.org/)
- [ArduinoでJSONを扱う方法](https://www.autonomation.ir/voice/control-your-arduino-with-an-ir-remote-sensor)
- [ArduinoJsonのドキュメント](https://arduinojson.org/v6/api/jsondocument/)