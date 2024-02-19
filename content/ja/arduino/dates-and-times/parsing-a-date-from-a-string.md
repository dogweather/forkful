---
aliases:
- /ja/arduino/parsing-a-date-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:25.800626-07:00
description: "\u2026"
lastmod: 2024-02-18 23:08:55.161661
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？

Arduinoで文字列から日付を解析することは、テキスト表現から日付成分（年、月、日）を抽出して変換し、スケッチ内での時間管理、比較、または操作に利用できる形式にすることを含みます。プログラマーは、リアルタイムクロック、ロガー、またはウェブAPIやユーザーインターフェースからの入力を処理する際に、日付が読みやすい形式で提示される場合に、このタスクを頻繁に実行します。

## 方法：

サードパーティのライブラリを使用せずに直接行う方法：

```cpp
#include <Wire.h>
#include <RTClib.h>

void setup() {
  Serial.begin(9600);
  // YYYY-MM-DD形式の例示日付文字列
  String dateString = "2023-04-01"; 

  int year = dateString.substring(0, 4).toInt();
  int month = dateString.substring(5, 7).toInt();
  int day = dateString.substring(8, 10).toInt();

  // 解析した成分でDateTimeオブジェクトを初期化
  DateTime parsedDate(year, month, day);
  
  Serial.print("解析した日付: ");
  Serial.print(parsedDate.year(), DEC);
  Serial.print("/");
  Serial.print(parsedDate.month(), DEC);
  Serial.print("/");
  Serial.println(parsedDate.day(), DEC);
}

void loop() {}
```

出力例：
```
解析した日付: 2023/4/1
```

サードパーティのライブラリを使用する方法（*ArduinoJson* を使用し、JSON応答から日付を取得するなど、より複雑な解析シナリオのために）：

まず、Arduino Library Managerを通じてArduinoJsonライブラリをインストールします。

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // JSON応答をシミュレート
  String jsonResponse = "{\"date\":\"2023-07-19\"}";
  StaticJsonDocument<200> doc;
  deserializeJson(doc, jsonResponse);

  // 日付文字列を抽出
  const char* date = doc["date"];

  // 以前と同じように文字列から日付を解析
  int year = String(date).substring(0, 4).toInt();
  int month = String(date).substring(5, 7).toInt();
  int day = String(date).substring(8, 10).toInt();
  
  Serial.print("JSONから解析した日付: ");
  Serial.print(year);
  Serial.print("/");
  Serial.print(month);
  Serial.print("/");
  Serial.println(day);
}

void loop() {}
```

出力例：
```
JSONから解析した日付: 2023/7/19
```
