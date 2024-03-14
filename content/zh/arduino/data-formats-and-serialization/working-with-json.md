---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:46.877312-07:00
description: "JSON\uFF0C\u5373JavaScript\u5BF9\u8C61\u8868\u793A\u6CD5\uFF0C\u662F\
  \u4E00\u79CD\u8F7B\u91CF\u7EA7\u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\uFF0C\u975E\
  \u5E38\u9002\u7528\u4E8EArduino\u9879\u76EE\u4E2D\u7684\u6570\u636E\u5B58\u50A8\u6216\
  \u914D\u7F6E\u6587\u4EF6\u3002\u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\u4F7F\u7528\u5B83\
  \uFF0C\u662F\u56E0\u4E3A\u5B83\u5728\u5404\u79CD\u7F16\u7A0B\u73AF\u5883\uFF08\u5305\
  \u62ECArduino\uFF09\u4E2D\u7684\u7B80\u5355\u6027\u548C\u53EF\u8BFB\u6027\uFF0C\u4F7F\
  \u5F97\u4E0EWeb APIs\u6216\u5176\u4ED6\u7CFB\u7EDF\u8FDB\u884C\u65E0\u7F1D\u6570\
  \u636E\u4EA4\u6362\u6210\u4E3A\u53EF\u80FD\u3002"
lastmod: '2024-03-13T22:44:48.087798-06:00'
model: gpt-4-0125-preview
summary: "JSON\uFF0C\u5373JavaScript\u5BF9\u8C61\u8868\u793A\u6CD5\uFF0C\u662F\u4E00\
  \u79CD\u8F7B\u91CF\u7EA7\u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\uFF0C\u975E\u5E38\
  \u9002\u7528\u4E8EArduino\u9879\u76EE\u4E2D\u7684\u6570\u636E\u5B58\u50A8\u6216\u914D\
  \u7F6E\u6587\u4EF6\u3002\u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\u4F7F\u7528\u5B83\uFF0C\
  \u662F\u56E0\u4E3A\u5B83\u5728\u5404\u79CD\u7F16\u7A0B\u73AF\u5883\uFF08\u5305\u62EC\
  Arduino\uFF09\u4E2D\u7684\u7B80\u5355\u6027\u548C\u53EF\u8BFB\u6027\uFF0C\u4F7F\u5F97\
  \u4E0EWeb APIs\u6216\u5176\u4ED6\u7CFB\u7EDF\u8FDB\u884C\u65E0\u7F1D\u6570\u636E\
  \u4EA4\u6362\u6210\u4E3A\u53EF\u80FD\u3002"
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
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
