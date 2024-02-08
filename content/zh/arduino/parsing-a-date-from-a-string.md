---
title:                "从字符串解析日期"
aliases:
- zh/arduino/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:13:33.902891-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串解析日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
