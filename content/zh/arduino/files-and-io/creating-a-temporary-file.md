---
aliases:
- /zh/arduino/creating-a-temporary-file/
date: 2024-01-20 17:39:58.197298-07:00
description: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6\u662F\u6307\u751F\u6210\u4E00\u4E2A\
  \u77ED\u671F\u5B58\u5728\u7684\u6587\u4EF6\uFF0C\u901A\u5E38\u7528\u4E8E\u5904\u7406\
  \u4E34\u65F6\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\
  \u907F\u514D\u5728\u957F\u671F\u5B58\u50A8\u4E2D\u4EA7\u751F\u6DF7\u4E71\uFF0C\u6216\
  \u8005\u56E0\u4E3A\u5B83\u4EEC\u53EA\u9700\u8981\u90A3\u4E9B\u6570\u636E\u4E00\u6B21\
  \u6027\u5904\u7406\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.385990
model: gpt-4-1106-preview
summary: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6\u662F\u6307\u751F\u6210\u4E00\u4E2A\
  \u77ED\u671F\u5B58\u5728\u7684\u6587\u4EF6\uFF0C\u901A\u5E38\u7528\u4E8E\u5904\u7406\
  \u4E34\u65F6\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\
  \u907F\u514D\u5728\u957F\u671F\u5B58\u50A8\u4E2D\u4EA7\u751F\u6DF7\u4E71\uFF0C\u6216\
  \u8005\u56E0\u4E3A\u5B83\u4EEC\u53EA\u9700\u8981\u90A3\u4E9B\u6570\u636E\u4E00\u6B21\
  \u6027\u5904\u7406\u3002"
title: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)

创建临时文件是指生成一个短期存在的文件，通常用于处理临时数据。程序员这么做是为了避免在长期存储中产生混乱，或者因为它们只需要那些数据一次性处理。

## How to: (怎么做：)

注意：Arduino通常用于控制硬件和与传感器交互，而不像传统编程环境那样直接创建文件。以下例子展示如何在SD卡上创建和删除临时文件，使用了SD库。

```cpp
#include <SD.h>

File tempFile;

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // 等待串行端口连接。仅对于 Leonardo, Micro, Zero 这样的板子需要。
  }

  if (!SD.begin(4)) {
    Serial.println("初始化SD卡失败");
    return;
  }

  tempFile = SD.open("temp.txt", FILE_WRITE);
  if (tempFile) {
    Serial.println("临时文件创建成功");
    tempFile.println("临时数据写入");
    tempFile.close(); // 关闭文件，保存数据。
  } else {
    Serial.println("创建临时文件失败");
  }
}

void loop() {
  // 一些其他代码
  
  // 如果需要删除临时文件
  if (SD.exists("temp.txt")) {
    SD.remove("temp.txt");
    Serial.println("临时文件已删除");
  }

  // 一些其他代码
}
```

## Deep Dive (深入探究)

临时文件历史悠久，用以处理临时信息流和中介数据。但在Arduino和其他嵌入式系统中，内存和存储限制让这个概念变得复杂。通常，你要么将数据放在临时变量（内存）中，要么写入EEPROM或外部存储，如SD卡，如我们上面的例子。相对于桌面系统，这些操作需要更精细的控制和出错处理。

在某些情况下，临时文件在Arduino上并不实用，你可能会选择使用其他数据结构比如队列或者缓冲区来临时存储数据。这些方法通常因为提供了更强的实时处理能力和更少的写入操作，从而保护了存储设备不被过度使用，何况写入操作可能相对较慢且占用更多的处理资源。

## See Also (另请参阅)

- SD库文档: https://www.arduino.cc/en/reference/SD
- Arduino存储选项: https://www.arduino.cc/en/Guide/Environment#toc8
- 文件系统相关文章: https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite
- EEPROM使用方法: https://www.arduino.cc/en/Tutorial/EEPROMReadWrite
