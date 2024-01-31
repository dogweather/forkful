---
title:                "处理JSON数据"
date:                  2024-01-19
html_title:           "Arduino: 处理JSON数据"
simple_title:         "处理JSON数据"

category:             "Arduino"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
处理JSON是指在Arduino中解析和生成JSON格式的数据。程序员这么做是为了方便数据交换，特别是在与APIs交互、物联网(IoT)项目中。

## How to: (如何操作)
首先，你需要安装一个叫做ArduinoJson的库。使用Arduino IDE的库管理器或直接下载然后添加到你的项目中。

```arduino
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);
  // JSON对象
  const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

  // 分配内存池
  StaticJsonDocument<200> doc;
  
  // 解析JSON
  DeserializationError error = deserializeJson(doc, json);

  // 解析检查
  if (error) {
    Serial.print("deserializeJson() failed with code ");
    Serial.println(error.c_str());
    return;
  }

  // 提取值
  const char* sensor = doc["sensor"];
  long time = doc["time"];
  float latitude = doc["data"][0];
  float longitude = doc["data"][1];

  // 打印值到串行监视器
  Serial.print("Sensor: ");
  Serial.println(sensor);
  Serial.print("Time: ");
  Serial.println(time);
  Serial.print("Latitude: ");
  Serial.println(latitude, 6);
  Serial.print("Longitude: ");
  Serial.println(longitude, 6);
}

void loop() {
  // 这里不做循环处理
}
```

代码将输出：
```
Sensor: gps
Time: 1351824120
Latitude: 48.756080
Longitude: 2.302038
```

## Deep Dive (深入了解)
JSON，即JavaScript Object Notation，是一种轻量级数据交换格式。在2001年被发明出来，很快成为了Web API的标准格式。尽管Arduino不运行JavaScript，但ArduinoJson库让处理JSON成为可能。对比XML等其他数据格式，JSON更紧凑、解析更快。ArduinoJson是Arduino生态里常用的JSON库，但还有其他的选择比如JsonStreamingParser。

## See Also (另请参阅)
- ArduinoJson官方文档（超详细）: https://arduinojson.org/
- ArduinoJson GitHub仓库（获取代码）: https://github.com/bblanchon/ArduinoJson
- JSON官方网站（学习JSON）: https://www.json.org/json-en.html
- Arduino官方网站（探索更多）: https://www.arduino.cc/
