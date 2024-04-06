---
date: 2024-01-20 17:39:58.197298-07:00
description: "How to: (\u600E\u4E48\u505A\uFF1A) \u6CE8\u610F\uFF1AArduino\u901A\u5E38\
  \u7528\u4E8E\u63A7\u5236\u786C\u4EF6\u548C\u4E0E\u4F20\u611F\u5668\u4EA4\u4E92\uFF0C\
  \u800C\u4E0D\u50CF\u4F20\u7EDF\u7F16\u7A0B\u73AF\u5883\u90A3\u6837\u76F4\u63A5\u521B\
  \u5EFA\u6587\u4EF6\u3002\u4EE5\u4E0B\u4F8B\u5B50\u5C55\u793A\u5982\u4F55\u5728SD\u5361\
  \u4E0A\u521B\u5EFA\u548C\u5220\u9664\u4E34\u65F6\u6587\u4EF6\uFF0C\u4F7F\u7528\u4E86\
  SD\u5E93\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.380903-06:00'
model: gpt-4-1106-preview
summary: "(\u600E\u4E48\u505A\uFF1A) \u6CE8\u610F\uFF1AArduino\u901A\u5E38\u7528\u4E8E\
  \u63A7\u5236\u786C\u4EF6\u548C\u4E0E\u4F20\u611F\u5668\u4EA4\u4E92\uFF0C\u800C\u4E0D\
  \u50CF\u4F20\u7EDF\u7F16\u7A0B\u73AF\u5883\u90A3\u6837\u76F4\u63A5\u521B\u5EFA\u6587\
  \u4EF6\u3002\u4EE5\u4E0B\u4F8B\u5B50\u5C55\u793A\u5982\u4F55\u5728SD\u5361\u4E0A\
  \u521B\u5EFA\u548C\u5220\u9664\u4E34\u65F6\u6587\u4EF6\uFF0C\u4F7F\u7528\u4E86SD\u5E93\
  \u3002"
title: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6"
weight: 21
---

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
