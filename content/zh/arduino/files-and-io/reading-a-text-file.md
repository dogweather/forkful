---
aliases:
- /zh/arduino/reading-a-text-file/
date: 2024-01-20 17:53:46.289134-07:00
description: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u5C31\u662F\u8BA9\u4F60\u7684Arduino\u8BBE\
  \u5907\u80FD\u7406\u89E3\u548C\u4F7F\u7528\u5B58\u50A8\u5728\u6587\u4EF6\u4E2D\u7684\
  \u6570\u636E\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u4ECE\u5B58\
  \u50A8\u5361\u4E2D\u5BFC\u5165\u914D\u7F6E\u53C2\u6570\u6216\u5904\u7406\u5B58\u50A8\
  \u7684\u4FE1\u606F\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.384192
model: gpt-4-1106-preview
summary: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u5C31\u662F\u8BA9\u4F60\u7684Arduino\u8BBE\
  \u5907\u80FD\u7406\u89E3\u548C\u4F7F\u7528\u5B58\u50A8\u5728\u6587\u4EF6\u4E2D\u7684\
  \u6570\u636E\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u4ECE\u5B58\
  \u50A8\u5361\u4E2D\u5BFC\u5165\u914D\u7F6E\u53C2\u6570\u6216\u5904\u7406\u5B58\u50A8\
  \u7684\u4FE1\u606F\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## What & Why? 什么与为什么？

读取文本文件就是让你的Arduino设备能理解和使用存储在文件中的数据。程序员这么做是为了从存储卡中导入配置参数或处理存储的信息。

## How to: 怎么做？

首先，确保你的Arduino板子连着SD卡。使用SD.h库来访问存储在SD卡上的文件。

```Arduino
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("SD card initialization failed!");
    return;
  }
  myFile = SD.open("test.txt");
  if (myFile) {
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  } else {
    Serial.println("Error opening test.txt");
  }
}

void loop() {
  // Nothing here
}
```

输出会是test.txt文件内容的直接打印。

## Deep Dive 深入探究

实际上，Arduino读取文件不是个新鲜事，而是计算机编程的老活儿。另外，你也可以用其他方法，比如EEPROM或SPIFFS（对于ESP8266和ESP32平台）。读取一个文本文件时，重要的细节包括文件的打开、逐字节的读取、错误处理以及最后关闭文件。确保路径正确和错误处理是关键步骤，以避免程序崩溃。

## See Also 相关资源

- Arduino SD Library Documentation: https://www.arduino.cc/en/Reference/SD
- Arduino File IO: https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite
- SPIFFS Documentation for ESP8266/ESP32: https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/storage/spiffs.html
