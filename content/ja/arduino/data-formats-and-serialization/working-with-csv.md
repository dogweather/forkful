---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:20.558564-07:00
description: "Arduino\u3067CSV\uFF08Comma-Separated\u2026"
lastmod: '2024-03-13T22:44:42.524707-06:00'
model: gpt-4-0125-preview
summary: "Arduino\u3067CSV\uFF08Comma-Separated Values\u3001\u30AB\u30F3\u30DE\u533A\
  \u5207\u308A\u5024\uFF09\u30D5\u30A1\u30A4\u30EB\u3092\u6271\u3046\u3053\u3068\u306F\
  \u3001\u901A\u5E38SD\u30AB\u30FC\u30C9\u306B\u4FDD\u5B58\u3055\u308C\u305FCSV\u30D5\
  \u30A1\u30A4\u30EB\u304B\u3089\u306E\u8AAD\u307F\u53D6\u308A\u3068\u66F8\u304D\u8FBC\
  \u307F\u3092\u542B\u307F\u3001\u30C7\u30FC\u30BF\u306E\u30ED\u30B0\u8A18\u9332\u3001\
  \u8A2D\u5B9A\u306E\u69CB\u6210\u306A\u3069\u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\
  \u3002\u305D\u306E\u30B7\u30F3\u30D7\u30EB\u3055\u3068\u30D7\u30E9\u30C3\u30C8\u30D5\
  \u30A9\u30FC\u30E0\u9593\u3067\u306E\u5E83\u7BC4\u306A\u63A1\u7528\u306E\u305F\u3081\
  \u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3057\u3070\u3057\u3070\u30BB\u30F3\
  \u30B5\u30FC\u30C7\u30FC\u30BF\u306E\u53CE\u96C6\u3001\u8A2D\u5B9A\u30D1\u30E9\u30E1\
  \u30FC\u30BF\u306E\u4FDD\u5B58\u3001\u307E\u305F\u306F\u4ED6\u306E\u30B7\u30B9\u30C6\
  \u30E0\u3068\u306E\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30FC\u30B9\u7528\u306BCSV\u3092\
  \u6271\u3044\u307E\u3059\u3002."
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 何となぜ？
ArduinoでCSV（Comma-Separated Values、カンマ区切り値）ファイルを扱うことは、通常SDカードに保存されたCSVファイルからの読み取りと書き込みを含み、データのログ記録、設定の構成などを可能にします。そのシンプルさとプラットフォーム間での広範な採用のため、プログラマーはしばしばセンサーデータの収集、設定パラメータの保存、または他のシステムとのインターフェース用にCSVを扱います。

## 方法：
ArduinoにはCSVファイルを処理するための組み込みライブラリはありませんが、`SD`および`SPI`ライブラリを使用してSDカード上のファイルにアクセスし、基本的な文字列操作技術を使用してCSVデータを解析または生成することができます。より複雑なCSVの操作を扱う場合には、第三者のライブラリ`ArduinoCSV`が解析と書き込みを容易にするために利用可能です。

**SDカードからのCSVデータの読み取り：**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      Serial.println(dataLine); // CSVラインを印刷
    }
    dataFile.close();
  } else {
    Serial.println("Error opening data.csv");
  }
}

void loop() {
  // この例では使用されない
}
```
*サンプル出力：*
```
SensorID, Timestamp, Value
1, 1597840923, 23.5
2, 1597840987, 22.4
```

**SDカードへのCSVデータの書き込み：**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  File dataFile = SD.open("output.csv", FILE_WRITE);
  if (dataFile) {
    dataFile.println("SensorID, Timestamp, Value"); // CSVヘッダー
    dataFile.println("1, 1597840923, 23.5"); // 例のデータ行
    dataFile.close();
    Serial.println("Data written");
  } else {
    Serial.println("Error opening output.csv");
  }
}

void loop() {
  // この例では使用されない
}
```
*サンプル出力：*
```
Data written
```

**ArduinoCSVを使用した解析：**
複雑なCSVファイルを扱う場合、`ArduinoCSV`ライブラリは解析作業を大幅に簡略化することができます。この例ではすでに`ArduinoCSV`ライブラリをインストールしていることを前提としています。

```cpp
#include <SPI.h>
#include <SD.h>
#include <ArduinoCSV.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    CSVParser parser;
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      if (parser.parseLine(dataLine)) {
        for (int i = 0; i < parser.count(); i++) {
          Serial.print(parser.getField(i)); // 各フィールドを印刷
          if (i < parser.count() - 1) {
            Serial.print(", ");
          }
        }
        Serial.println();
      }
    }
    dataFile.close();
  } else {
    Serial.println("Error opening data.csv");
  }
}

void loop() {
  // この例では使用されない
}
```
*サンプル出力：*
```
SensorID,  Timestamp,  Value
1,  1597840923,  23.5
2,  1597840987,  22.4
```
これらの例では、SDカード上のCSVファイルから読み取りと書き込みを行うことで、Arduinoプロジェクトは簡単にデータを収集し、設定を保存し、または他のアプリケーションとデータを交換できます。これは、広くアクセス可能な形式で行うことができます。
