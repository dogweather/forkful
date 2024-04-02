---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:22.359482-07:00
description: "\u5728Arduino\u4E2D\u5904\u7406CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\
  \uFF09\u6587\u4EF6\uFF0C\u6D89\u53CA\u4ECESD\u5361\u4E0A\u901A\u5E38\u5B58\u50A8\
  \u7684CSV\u6587\u4EF6\u4E2D\u8BFB\u53D6\u548C\u5199\u5165\uFF0C\u4F7F\u6570\u636E\
  \u8BB0\u5F55\u3001\u914D\u7F6E\u8BBE\u7F6E\u7B49\u6210\u4E3A\u53EF\u80FD\u3002\u7A0B\
  \u5E8F\u5458\u5E38\u5E38\u5904\u7406CSV\u7528\u4E8E\u4F20\u611F\u5668\u6570\u636E\
  \u6536\u96C6\u3001\u914D\u7F6E\u53C2\u6570\u5B58\u50A8\uFF0C\u6216\u4E0E\u5176\u4ED6\
  \u7CFB\u7EDF\u63A5\u53E3\uFF0C\u56E0\u4E3A\u5B83\u7684\u7B80\u5355\u6027\u4EE5\u53CA\
  \u8DE8\u5E73\u53F0\u7684\u5E7F\u6CDB\u91C7\u7528\u3002"
lastmod: '2024-03-13T22:44:48.089226-06:00'
model: gpt-4-0125-preview
summary: "\u5728Arduino\u4E2D\u5904\u7406CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\
  \u6587\u4EF6\uFF0C\u6D89\u53CA\u4ECESD\u5361\u4E0A\u901A\u5E38\u5B58\u50A8\u7684\
  CSV\u6587\u4EF6\u4E2D\u8BFB\u53D6\u548C\u5199\u5165\uFF0C\u4F7F\u6570\u636E\u8BB0\
  \u5F55\u3001\u914D\u7F6E\u8BBE\u7F6E\u7B49\u6210\u4E3A\u53EF\u80FD\u3002\u7A0B\u5E8F\
  \u5458\u5E38\u5E38\u5904\u7406CSV\u7528\u4E8E\u4F20\u611F\u5668\u6570\u636E\u6536\
  \u96C6\u3001\u914D\u7F6E\u53C2\u6570\u5B58\u50A8\uFF0C\u6216\u4E0E\u5176\u4ED6\u7CFB\
  \u7EDF\u63A5\u53E3\uFF0C\u56E0\u4E3A\u5B83\u7684\u7B80\u5355\u6027\u4EE5\u53CA\u8DE8\
  \u5E73\u53F0\u7684\u5E7F\u6CDB\u91C7\u7528\u3002"
title: "\u5904\u7406CSV\u6587\u4EF6"
weight: 37
---

## 什么 & 为什么？
在Arduino中处理CSV（逗号分隔值）文件，涉及从SD卡上通常存储的CSV文件中读取和写入，使数据记录、配置设置等成为可能。程序员常常处理CSV用于传感器数据收集、配置参数存储，或与其他系统接口，因为它的简单性以及跨平台的广泛采用。

## 如何操作：
Arduino没有内置专门用于处理CSV文件的库，但你可以使用`SD`和`SPI`库来访问SD卡上的文件，然后使用基本的字符串操作技术来解析或生成CSV数据。当处理更复杂的CSV操作时，可以利用第三方库`ArduinoCSV`来更简单地进行解析和写入。

**从SD卡读取CSV数据：**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("初始化失败！");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      Serial.println(dataLine); // 打印CSV行
    }
    dataFile.close();
  } else {
    Serial.println("打开data.csv失败");
  }
}

void loop() {
  // 在此示例中未使用
}
```
*示例输出：*
```
SensorID, Timestamp, Value
1, 1597840923, 23.5
2, 1597840987, 22.4
```

**将CSV数据写入SD卡：**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("初始化失败！");
    return;
  }
  File dataFile = SD.open("output.csv", FILE_WRITE);
  if (dataFile) {
    dataFile.println("SensorID, Timestamp, Value"); // CSV头
    dataFile.println("1, 1597840923, 23.5"); // 示例数据行
    dataFile.close();
    Serial.println("数据已写入");
  } else {
    Serial.println("打开output.csv失败");
  }
}

void loop() {
  // 在此示例中未使用
}
```
*示例输出：*
```
数据已写入
```

**使用ArduinoCSV进行解析：**
如果处理复杂的CSV文件，`ArduinoCSV`库可以大大简化解析工作。此示例假设你已经安装了`ArduinoCSV`库。

```cpp
#include <SPI.h>
#include <SD.h>
#include <ArduinoCSV.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("初始化失败！");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    CSVParser parser;
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      if (parser.parseLine(dataLine)) {
        for (int i = 0; i < parser.count(); i++) {
          Serial.print(parser.getField(i)); // 打印每个字段
          if (i < parser.count() - 1) {
            Serial.print(", ");
          }
        }
        Serial.println();
      }
    }
    dataFile.close();
  } else {
    Serial.println("打开data.csv失败");
  }
}

void loop() {
  // 在此示例中未使用
}
```
*示例输出：*
```
SensorID,  Timestamp,  Value
1,  1597840923,  23.5
2,  1597840987,  22.4
```
通过这些示例，通过从SD卡上的CSV文件读取和写入，Arduino项目可以轻松地收集数据、存储配置设置，或与其他应用程序交换数据，使用一种普遍可访问的格式。
