---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:22.359482-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Arduino\u6CA1\u6709\u5185\u7F6E\u4E13\
  \u95E8\u7528\u4E8E\u5904\u7406CSV\u6587\u4EF6\u7684\u5E93\uFF0C\u4F46\u4F60\u53EF\
  \u4EE5\u4F7F\u7528`SD`\u548C`SPI`\u5E93\u6765\u8BBF\u95EESD\u5361\u4E0A\u7684\u6587\
  \u4EF6\uFF0C\u7136\u540E\u4F7F\u7528\u57FA\u672C\u7684\u5B57\u7B26\u4E32\u64CD\u4F5C\
  \u6280\u672F\u6765\u89E3\u6790\u6216\u751F\u6210CSV\u6570\u636E\u3002\u5F53\u5904\
  \u7406\u66F4\u590D\u6742\u7684CSV\u64CD\u4F5C\u65F6\uFF0C\u53EF\u4EE5\u5229\u7528\
  \u7B2C\u4E09\u65B9\u5E93`ArduinoCSV`\u6765\u66F4\u7B80\u5355\u5730\u8FDB\u884C\u89E3\
  \u6790\u548C\u5199\u5165\u3002 **\u4ECESD\u5361\u8BFB\u53D6CSV\u6570\u636E\uFF1A\
  **."
lastmod: '2024-04-05T22:38:47.244619-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Arduino\u6CA1\u6709\u5185\u7F6E\u4E13\u95E8\
  \u7528\u4E8E\u5904\u7406CSV\u6587\u4EF6\u7684\u5E93\uFF0C\u4F46\u4F60\u53EF\u4EE5\
  \u4F7F\u7528`SD`\u548C`SPI`\u5E93\u6765\u8BBF\u95EESD\u5361\u4E0A\u7684\u6587\u4EF6\
  \uFF0C\u7136\u540E\u4F7F\u7528\u57FA\u672C\u7684\u5B57\u7B26\u4E32\u64CD\u4F5C\u6280\
  \u672F\u6765\u89E3\u6790\u6216\u751F\u6210CSV\u6570\u636E\u3002\u5F53\u5904\u7406\
  \u66F4\u590D\u6742\u7684CSV\u64CD\u4F5C\u65F6\uFF0C\u53EF\u4EE5\u5229\u7528\u7B2C\
  \u4E09\u65B9\u5E93`ArduinoCSV`\u6765\u66F4\u7B80\u5355\u5730\u8FDB\u884C\u89E3\u6790\
  \u548C\u5199\u5165\u3002 **\u4ECESD\u5361\u8BFB\u53D6CSV\u6570\u636E\uFF1A**."
title: "\u5904\u7406CSV\u6587\u4EF6"
weight: 37
---

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
