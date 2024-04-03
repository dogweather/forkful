---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:20.558564-07:00
description: "\u65B9\u6CD5\uFF1A\u2026"
lastmod: '2024-03-13T22:44:42.524707-06:00'
model: gpt-4-0125-preview
summary: "Arduino\u306B\u306FCSV\u30D5\u30A1\u30A4\u30EB\u3092\u51E6\u7406\u3059\u308B\
  \u305F\u3081\u306E\u7D44\u307F\u8FBC\u307F\u30E9\u30A4\u30D6\u30E9\u30EA\u306F\u3042\
  \u308A\u307E\u305B\u3093\u304C\u3001`SD`\u304A\u3088\u3073`SPI`\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3092\u4F7F\u7528\u3057\u3066SD\u30AB\u30FC\u30C9\u4E0A\u306E\u30D5\
  \u30A1\u30A4\u30EB\u306B\u30A2\u30AF\u30BB\u30B9\u3057\u3001\u57FA\u672C\u7684\u306A\
  \u6587\u5B57\u5217\u64CD\u4F5C\u6280\u8853\u3092\u4F7F\u7528\u3057\u3066CSV\u30C7\
  \u30FC\u30BF\u3092\u89E3\u6790\u307E\u305F\u306F\u751F\u6210\u3059\u308B\u3053\u3068\
  \u304C\u3067\u304D\u307E\u3059\u3002\u3088\u308A\u8907\u96D1\u306ACSV\u306E\u64CD\
  \u4F5C\u3092\u6271\u3046\u5834\u5408\u306B\u306F\u3001\u7B2C\u4E09\u8005\u306E\u30E9\
  \u30A4\u30D6\u30E9\u30EA`ArduinoCSV`\u304C\u89E3\u6790\u3068\u66F8\u304D\u8FBC\u307F\
  \u3092\u5BB9\u6613\u306B\u3059\u308B\u305F\u3081\u306B\u5229\u7528\u53EF\u80FD\u3067\
  \u3059."
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

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
