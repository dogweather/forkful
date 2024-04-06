---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:46.877312-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u5728Arduino\u4E2D\u64CD\u4F5CJSON\uFF0C\
  `ArduinoJson`\u5E93\u662F\u4E00\u4E2A\u56E0\u5176\u6613\u7528\u6027\u548C\u6548\u7387\
  \u800C\u6D41\u884C\u7684\u9009\u62E9\u3002\u5B83\u5141\u8BB8\u89E3\u6790JSON\u5B57\
  \u7B26\u4E32\uFF0C\u4FEE\u6539\u5B83\u4EEC\uFF0C\u5E76\u5C06\u5BF9\u8C61\u5E8F\u5217\
  \u5316\u56DEJSON\u5B57\u7B26\u4E32\u3002\u4EE5\u4E0B\u662F\u5982\u4F55\u4F7F\u7528\
  \u5B83\uFF1A 1. **\u5B89\u88C5ArduinoJson\u5E93**\uFF1A\u4F7F\u7528Arduino\u2026"
lastmod: '2024-04-05T21:53:48.383591-06:00'
model: gpt-4-0125-preview
summary: "\u5728Arduino\u4E2D\u64CD\u4F5CJSON\uFF0C`ArduinoJson`\u5E93\u662F\u4E00\
  \u4E2A\u56E0\u5176\u6613\u7528\u6027\u548C\u6548\u7387\u800C\u6D41\u884C\u7684\u9009\
  \u62E9\u3002\u5B83\u5141\u8BB8\u89E3\u6790JSON\u5B57\u7B26\u4E32\uFF0C\u4FEE\u6539\
  \u5B83\u4EEC\uFF0C\u5E76\u5C06\u5BF9\u8C61\u5E8F\u5217\u5316\u56DEJSON\u5B57\u7B26\
  \u4E32\u3002\u4EE5\u4E0B\u662F\u5982\u4F55\u4F7F\u7528\u5B83\uFF1A 1."
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
weight: 38
---

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
