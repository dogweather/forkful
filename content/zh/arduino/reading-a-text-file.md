---
title:                "读取文本文件"
html_title:           "Kotlin: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 是什么 & 为什么？

读取文本文件是一个获取并解析存储在文本文件中的数据的过程。程序员之所以需要做这个，主要是为了处理大量的数据，或者从其他程序中获取数据。

## 如何操作:

使用Arduino读取文本文件有多个方式，这里我们以SD卡模块读取的方式为例。

```Arduino
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(4)) {
    Serial.println("初始化失败！");
    return;
  }
  
  myFile = SD.open("test.txt");
  
  if (myFile) {
    Serial.println("test.txt:");

    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    
    myFile.close();
  } else {
    Serial.println("打开文件错误");
  }
}

void loop() {
  // put your main code here, to run repeatedly:

}
```
完成以上的代码后，你就可以在串口监视器中观察到test.txt文件中的内容了。

## 深入解析:

读取文本文件在许多编程语言中都是一个基本的操作过程，在早期的操作系统如Unix中，文本文件已经被广泛用于配置文件，日志文件等。在Arduino中我们有多种方式读取文本文件,例如通过串口、以太网或者Wifi从服务器获取，通过SPI接口从Flash或者SD卡获取等。每一种方式都有其适用的场合和优缺点。

## 参考链接:

1. 所有的SD库的函数和例程: https://www.arduino.cc/en/Reference/SD
2. 使用SPI接口读取Flash的详细教程: https://learn.adafruit.com/adafruit-data-logger-shield
3. 如何使用以太网或者Wifi从服务器获取文本文件: https://www.arduino.cc/en/Tutorial/WebClient