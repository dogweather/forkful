---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:25.800626-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.510270-06:00'
model: gpt-4-0125-preview
summary: "Arduino\u3067\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\
  \u3059\u308B\u3053\u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u8868\u73FE\u304B\u3089\
  \u65E5\u4ED8\u6210\u5206\uFF08\u5E74\u3001\u6708\u3001\u65E5\uFF09\u3092\u62BD\u51FA\
  \u3057\u3066\u5909\u63DB\u3057\u3001\u30B9\u30B1\u30C3\u30C1\u5185\u3067\u306E\u6642\
  \u9593\u7BA1\u7406\u3001\u6BD4\u8F03\u3001\u307E\u305F\u306F\u64CD\u4F5C\u306B\u5229\
  \u7528\u3067\u304D\u308B\u5F62\u5F0F\u306B\u3059\u308B\u3053\u3068\u3092\u542B\u307F\
  \u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30EA\u30A2\u30EB\
  \u30BF\u30A4\u30E0\u30AF\u30ED\u30C3\u30AF\u3001\u30ED\u30AC\u30FC\u3001\u307E\u305F\
  \u306F\u30A6\u30A7\u30D6API\u3084\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\
  \u30D5\u30A7\u30FC\u30B9\u304B\u3089\u306E\u5165\u529B\u3092\u51E6\u7406\u3059\u308B\
  \u969B\u306B\u3001\u65E5\u4ED8\u304C\u8AAD\u307F\u3084\u3059\u3044\u5F62\u5F0F\u3067\
  \u63D0\u793A\u3055\u308C\u308B\u5834\u5408\u306B\u3001\u3053\u306E\u30BF\u30B9\u30AF\
  \u3092\u983B\u7E41\u306B\u5B9F\u884C\u3057\u307E\u3059\u3002."
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
weight: 30
---

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
