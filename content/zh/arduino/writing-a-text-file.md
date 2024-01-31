---
title:                "编写文本文件"
date:                  2024-01-19
simple_title:         "编写文本文件"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 什么&为什么？
写文本文件就是在SD卡或其他存储器中保存文本信息。程序员这么做是因为需要记录数据（如传感器读数）或保存程序设置。

## 怎么做：
```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  while(!Serial) {
    ; // 等待串行端口连上。
  }
  
  if (!SD.begin(4)) { // CS Pin 4
    Serial.println("初始化SD卡失败");
    return;
  }
  Serial.println("初始化SD卡成功");
  
  myFile = SD.open("test.txt", FILE_WRITE);
  if (myFile) {
    myFile.println("Hello, world!");
    myFile.close(); // 关闭文件
    Serial.println("写入成功");
  } else {
    Serial.println("打开文件失败");
  }
}

void loop() {
  // 不需要循环操作
}
```
样本输出：
```
初始化SD卡成功
写入成功
```

## 深入探索：
写文本文件在早期采用磁带和磁盘时就已经存在。在微控制器（如Arduino）中，通常使用SD卡。备选方案包括EEPROM或云服务。实现这个功能时需考虑文件系统（如FAT16/FAT32）和最大文件大小限制。

## 参见：
- Arduino SD 库：https://www.arduino.cc/en/Reference/SD
- SD 卡文件系统和格式化工具：https://www.sdcard.org/downloads/formatter/
- SPI 协议原理：https://en.wikipedia.org/wiki/Serial_Peripheral_Interface
