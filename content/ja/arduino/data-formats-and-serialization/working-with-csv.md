---
aliases:
- /ja/arduino/working-with-csv/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:20.558564-07:00
description: "Arduino\u3067CSV\uFF08Comma-Separated\u2026"
lastmod: 2024-02-18 23:08:55.172860
model: gpt-4-0125-preview
summary: "Arduino\u3067CSV\uFF08Comma-Separated\u2026"
title: "CSV\u3068\u306E\u4F5C\u696D"
---

{{< edit_this_page >}}

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
