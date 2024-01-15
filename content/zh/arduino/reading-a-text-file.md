---
title:                "读取文本文件"
html_title:           "Arduino: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

当我们编写程序时，经常需要从外部资源中读取数据，比如文本文件。读取文本文件可以让我们轻松获取需要处理的大量数据，从而提高开发效率。本文将介绍如何在Arduino中读取文本文件。

## 怎么做

为了在Arduino中读取文本文件，我们需要使用``SD``库和SD卡模块。首先，我们需要在头部添加``SD``库以及定义SD卡模块的引脚。

```
#include <SD.h> //引入SD库
#define SD_CS 10 //定义CS引脚（请根据实际情况修改）
```

然后，在``setup()``函数中，我们需要初始化SD卡模块。

```
void setup() {
  //初始化SD卡模块
  if (!SD.begin(SD_CS)) {
    Serial.println("SD卡初始化失败！");
    return;
  }
  Serial.println("SD卡初始化成功！");
}
```

接下来，在``loop()``函数中，我们可以使用``SD.open()``函数打开文本文件，并使用``readString()``函数读取文件中的内容。

```
void loop() {
  //打开文本文件
  File file = SD.open("test.txt");
  //读取文件内容
  String content = file.readString();
  Serial.println(content); //输出文件内容
  //关闭文件
  file.close();
}
```

代码执行后，我们可以在串口监视器中看到文本文件中的内容被成功读取并输出到串口。

## 深入探讨

在Arduino中，我们可以使用``File``类配合``SD``库来读取文本文件。``File``类是SD库中的一个内置类，它可以用来打开、读取和写入SD卡上的文件。而``SD.open()``函数则可以接受文本文件的名称作为参数，并返回一个``File``对象，从而让我们可以对文件进行读取操作。

## 参考文章

- [Arduino官方文档 - SD库](https://www.arduino.cc/en/Reference/SD)
- [ladyada.net - SD卡读取教程](https://learn.adafruit.com/adafruit-arduino-lesson-5-the-serial-monitor/reading-a-data-file)