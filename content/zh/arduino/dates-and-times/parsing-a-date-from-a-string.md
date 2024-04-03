---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:33.902891-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u4E0D\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\
  \u7684\u76F4\u63A5\u65B9\u6CD5\uFF1A."
lastmod: '2024-03-13T22:44:48.072832-06:00'
model: gpt-4-0125-preview
summary: "\u4E0D\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\u7684\u76F4\u63A5\u65B9\u6CD5\
  \uFF1A."
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
weight: 30
---

## 如何操作：
不使用第三方库的直接方法：

```cpp
#include <Wire.h>
#include <RTClib.h>

void setup() {
  Serial.begin(9600);
  // 示例日期字符串，格式为YYYY-MM-DD
  String dateString = "2023-04-01"; 

  int year = dateString.substring(0, 4).toInt();
  int month = dateString.substring(5, 7).toInt();
  int day = dateString.substring(8, 10).toInt();

  // 用解析的组件初始化一个DateTime对象
  DateTime parsedDate(year, month, day);
  
  Serial.print("解析日期：");
  Serial.print(parsedDate.year(), DEC);
  Serial.print("/");
  Serial.print(parsedDate.month(), DEC);
  Serial.print("/");
  Serial.println(parsedDate.day(), DEC);
}

void loop() {}
```

示例输出：
```
解析日期：2023/4/1
```

使用第三方库（*ArduinoJson* 用于更复杂的解析场景，例如从JSON响应中获取日期）：

首先，通过Arduino库管理器安装ArduinoJson库。

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // 模拟一个JSON响应
  String jsonResponse = "{\"date\":\"2023-07-19\"}";
  StaticJsonDocument<200> doc;
  deserializeJson(doc, jsonResponse);

  // 提取日期字符串
  const char* date = doc["date"];

  // 如之前所述从字符串解析日期
  int year = String(date).substring(0, 4).toInt();
  int month = String(date).substring(5, 7).toInt();
  int day = String(date).substring(8, 10).toInt();
  
  Serial.print("从JSON解析的日期：");
  Serial.print(year);
  Serial.print("/");
  Serial.print(month);
  Serial.print("/");
  Serial.println(day);
}

void loop() {}
```

示例输出：
```
从JSON解析的日期：2023/7/19
```
