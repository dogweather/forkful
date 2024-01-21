---
title:                "阅读文本文件"
date:                  2024-01-20T17:53:46.289134-07:00
model:                 gpt-4-1106-preview
simple_title:         "阅读文本文件"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/reading-a-text-file.md"
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