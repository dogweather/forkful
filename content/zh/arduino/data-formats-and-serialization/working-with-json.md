---
title:                "使用JSON进行编程"
aliases:
- /zh/arduino/working-with-json.md
date:                  2024-02-03T19:21:46.877312-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用JSON进行编程"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么?

JSON，即JavaScript对象表示法，是一种轻量级的数据交换格式，非常适用于Arduino项目中的数据存储或配置文件。程序员之所以使用它，是因为它在各种编程环境（包括Arduino）中的简单性和可读性，使得与Web APIs或其他系统进行无缝数据交换成为可能。

## 如何操作:

在Arduino中操作JSON，`ArduinoJson`库是一个因其易用性和效率而流行的选择。它允许解析JSON字符串，修改它们，并将对象序列化回JSON字符串。以下是如何使用它：

1. **安装ArduinoJson库**：使用Arduino IDE中的库管理器并安装"ArduinoJson"。

2. **反序列化一个JSON字符串**：以下是如何解析一个JSON字符串并提取值。

```cpp
#include <ArduinoJson.h>

const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

void setup() {
  Serial.begin(9600);
  StaticJsonDocument<200> doc; // 根据JSON文档调整大小
  DeserializationError error = deserializeJson(doc, json);

  if (error) {
    Serial.print(F("deserializeJson() 失败："));
    Serial.println(error.f_str());
    return;
  }

  const char* sensor = doc["sensor"]; // "gps"
  long time = doc["time"]; // 1351824120
  float latitude = doc["data"][0]; // 48.756080
  float longitude = doc["data"][1]; // 2.302038
  
  Serial.println(sensor);
  Serial.println(time);
  Serial.println(latitude, 6);
  Serial.println(longitude, 6);
}

void loop() {
  // 空循环
}
```

示例输出:

```
gps
1351824120
48.756080
2.302038
```

3. **序列化成一个JSON字符串**：以下是如何从数据创建一个JSON字符串。

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc; // 根据数据调整大小
  doc["sensor"] = "gps";
  doc["time"] = 1351824120;
  JsonArray data = doc.createNestedArray("data");
  data.add(48.756080);
  data.add(2.302038);

  serializeJson(doc, Serial);
}

void loop() {
  // 空循环
}
```

示例输出（为了易读性进行了格式化）:

```
{"sensor":"gps","time":1351824120,"data":[48.756080,2.302038]}
```

有效使用`ArduinoJson`库允许Arduino项目以人类可读的格式通信复杂的数据结构，促进了开发和与Web服务的集成。
