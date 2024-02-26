---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:33.902891-07:00
description: "\u5728Arduino\u4E2D\u89E3\u6790\u5B57\u7B26\u4E32\u4E2D\u7684\u65E5\u671F\
  \u6D89\u53CA\u4ECE\u6587\u672C\u8868\u793A\u4E2D\u63D0\u53D6\u548C\u8F6C\u6362\u65E5\
  \u671F\u7EC4\u4EF6\uFF08\u5E74\u3001\u6708\u3001\u65E5\uFF09\uFF0C\u8F6C\u6362\u6210\
  \u53EF\u4EE5\u7528\u4E8E\u65F6\u95F4\u8BB0\u5F55\u3001\u6BD4\u8F83\u6216\u5728\u8349\
  \u56FE\u4E2D\u8FDB\u884C\u64CD\u4F5C\u7684\u683C\u5F0F\u3002\u7F16\u7A0B\u4EBA\u5458\
  \u7ECF\u5E38\u6267\u884C\u6B64\u4EFB\u52A1\u4EE5\u4E0E\u5B9E\u65F6\u65F6\u949F\u3001\
  \u8BB0\u5F55\u5668\u63A5\u53E3\uFF0C\u6216\u5904\u7406\u6765\u81EA\u7F51\u7EDCAPI\u548C\
  \u7528\u6237\u754C\u9762\u7684\u8F93\u5165\uFF0C\u5176\u4E2D\u65E5\u671F\u53EF\u80FD\
  \u4EE5\u53EF\u8BFB\u683C\u5F0F\u5448\u73B0\u3002"
lastmod: '2024-02-25T18:49:45.639519-07:00'
model: gpt-4-0125-preview
summary: "\u5728Arduino\u4E2D\u89E3\u6790\u5B57\u7B26\u4E32\u4E2D\u7684\u65E5\u671F\
  \u6D89\u53CA\u4ECE\u6587\u672C\u8868\u793A\u4E2D\u63D0\u53D6\u548C\u8F6C\u6362\u65E5\
  \u671F\u7EC4\u4EF6\uFF08\u5E74\u3001\u6708\u3001\u65E5\uFF09\uFF0C\u8F6C\u6362\u6210\
  \u53EF\u4EE5\u7528\u4E8E\u65F6\u95F4\u8BB0\u5F55\u3001\u6BD4\u8F83\u6216\u5728\u8349\
  \u56FE\u4E2D\u8FDB\u884C\u64CD\u4F5C\u7684\u683C\u5F0F\u3002\u7F16\u7A0B\u4EBA\u5458\
  \u7ECF\u5E38\u6267\u884C\u6B64\u4EFB\u52A1\u4EE5\u4E0E\u5B9E\u65F6\u65F6\u949F\u3001\
  \u8BB0\u5F55\u5668\u63A5\u53E3\uFF0C\u6216\u5904\u7406\u6765\u81EA\u7F51\u7EDCAPI\u548C\
  \u7528\u6237\u754C\u9762\u7684\u8F93\u5165\uFF0C\u5176\u4E2D\u65E5\u671F\u53EF\u80FD\
  \u4EE5\u53EF\u8BFB\u683C\u5F0F\u5448\u73B0\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在Arduino中解析字符串中的日期涉及从文本表示中提取和转换日期组件（年、月、日），转换成可以用于时间记录、比较或在草图中进行操作的格式。编程人员经常执行此任务以与实时时钟、记录器接口，或处理来自网络API和用户界面的输入，其中日期可能以可读格式呈现。

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
